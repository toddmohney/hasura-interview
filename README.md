# Hasura Interview

## Setting up a development environment

Development environment dependencies
- stack
- hlint
- docker, docker-compose

### Development tools

The development environment is run via `make` tasks. You can run the following
to see a list of all available tasks

```
make help
```

To build, test, and lint the Haskell codebase with dynamic re-compilation on file
changes, run the following command.

```
make devel
```

### Running the webserver locally

There is a make task which takes care of building the Haskell binary and
Docker image, then starting the container in a `docker-compose` environment.

Run this make task, then visit `http://localhost:3000`

```
make restart
```

**NOTE!**

This project assumes the Haskell binaries are produced from an Ubuntu 18.04 host.

**What? Why?**

For simplicity and development efficiency, we build the Haskell binary on the
host machine, then copy them into the Docker container. Since the Docker
container is based on Ubutnu 18.04, we can only guarantee the Haskell binary
will run within the container if the host machine's architecture is the same.

We could solve this problem by building the Haskell binary within the Docker
image. In a more production-project, we could use, say, our CI environment to
produce the Docker image.
