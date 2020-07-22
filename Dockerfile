FROM ubuntu:18.04

ENV DEBIAN_FRONTEND=noninteractive
RUN apt-get update && apt-get install -y \
      build-essential \
      curl \
      libpq-dev \
      locales \
      postgresql-client \
      software-properties-common \
      tzdata

RUN sed -i -e 's/# en_US.UTF-8 UTF-8/en_US.UTF-8 UTF-8/' /etc/locale.gen && locale-gen

ENV LANGUAGE=en_US.utf8
ENV LANG=en_US.utf8

RUN mkdir -p /usr/local/src/workspace

WORKDIR /usr/local/src/workspace

COPY ./public /usr/local/src/workspace/public/

COPY ./bin/* /usr/local/bin/

CMD ["server"]

