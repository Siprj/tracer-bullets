ARG UBUNTU_VERSION=20.04

FROM ubuntu:20.04 as bash-log-gen
COPY log.sh /
CMD ["/log.sh"]

# fluentd/Dockerfile
FROM fluent/fluentd:v1.14-debian-1 as fluent

# Use root account to use apt
USER root

# below RUN includes plugin as examples elasticsearch is not required
# you may customize including plugins as you wish
RUN buildDeps="sudo make gcc g++ libc-dev" \
 && apt-get update \
 && apt-get install -y --no-install-recommends $buildDeps iproute2 \
 && sudo gem install fluent-plugin-elasticsearch \
 && sudo gem sources --clear-all \
 && SUDO_FORCE_REMOVE=yes \
    apt-get purge -y --auto-remove \
                  -o APT::AutoRemove::RecommendsImportant=false \
                  $buildDeps \
 && rm -rf /var/lib/apt/lists/* \
 && rm -rf /tmp/* /var/tmp/* /usr/lib/ruby/gems/*/cache/*.gem \
 && rm -rf fluent.conf

COPY --from=fluent/fluentd:v1.14-debian-1 /bin/entrypoint.sh /bin/

USER fluent
