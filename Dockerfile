FROM couchdb:2.1.2

RUN apt-get update -y && apt-get install -y --no-install-recommends \
    build-essential \
    git \
    rebar \
  && rm -rf /var/lib/apt/lists/*

VOLUME /
