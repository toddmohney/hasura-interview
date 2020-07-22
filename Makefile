.DEFAULT_GOAL := help


SERVER_DEVELOPMENT_IMAGE_TAG=hasura-interview-server
SERVER_PRODUCTION_IMAGE_TAG=registry.heroku.com/hasura-interview/web


.PHONY: help
help:
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'


.PHONY: devel
devel: ## Builds and tests upon file change
	@stack test \
		--fast \
		--file-watch \
		--no-keep-going \
		--exec="$(MAKE) lint"


.PHONY: lint
lint: ## Runs code quality and style consistency checks
	@hlint lint \
		./src \
		./lib


.PHONY: restart
restart: ## Rebuilds and restarts our server
	@$(MAKE) build-server
	@docker-compose rm -sf server
	@docker-compose up -d --remove-orphans


.PHONY: build-server
build-server: ## Builds a new Docker image for our server
	@stack build
	@cp \
		$$(stack path | awk '/local-install/ {print $$2}')/bin/* \
		./bin
	@docker-compose build server


.PHONY: deploy
deploy: ## Builds and deploys images to Heroku
	# heroku login
	heroku container:login
	# build our server image
	$(MAKE) build-server
	# build our production server image
	docker build \
		--build-arg BASE_IMAGE="$(SERVER_DEVELOPMENT_IMAGE_TAG)" \
		-t $(SERVER_PRODUCTION_IMAGE_TAG) \
		-f ./Dockerfile.web .
	# push our release image to the image repository
	docker push $(SERVER_PRODUCTION_IMAGE_TAG)
	# release our changes
	heroku container:release web -a hasura-interview
