# [OpenTelemetry](https://opentelemetry.io/) experiments

These experiments are looking into how one would go about setting up
OpenTelemetry in some dockerized application. Also it is interesteing how the
logs could be exported into some log storage. All this could be done by using
the `opentelemetry-collector`, which can receive variety of log/trace/metric
formats, and transform them into common representation. Data can then be send
to various storage backends. Some of the storage backends are elasticsearch,
inflixdb and loki.

Warning: The security and encryption wasn't part of the experiments.

First part of the experiment was about getting some data into the (chosen)
storage. For that, the stdout/stderr from the docker container was used
as the "easy" data source. See the bash `log.sh` script :). And associated
`docker-compose.yml` and `Dockerfile` files to see more about the individual
setups.

The docker-compose supports multiple logging drivers but most of them are
for proprietary platforms like `gcplogs` or `awslogs`. But there are some
that are more or less open like `syslog` or `fluentd`. After some initial
try and error, the us of `fluentd` was decided.

Note: In hindsight, it is clear it is better to use `syslog`, because the
`syslog` receiver has more features regarding TLS and authentication for now.

The used logging driver most of the experiments was the `fluentd` driver,
because it seamed to be the most simple solution at the time. That
said, the tricky part of the `fluentd` logging driver was to make
everything start in correct order. Also it is important to understand
that docker logging driver uses different DNS from the one used by
containers. That means the `log-collector` needs to expose a port on
the host machine. Also if the container logging into the `log-collector`
starts sooner then the `log-collector` is able to open the required port,
then the whole start fails. This took surprisingly log to figure out. This
flag `fluentd-async-connect` switches the `fluentd` logging driver into
asynchronous mode and suddenly the whole docker-compose doesn't care about
the order in which containers are starting.

## 1. Experiment `influx` db, `grafana` and `fluentd` driver

In this case the provisioning was probably the most pleasant to work with
out of all the tried options. It is possible to configure all the access
account and keys simply by setting them using the environment variables,
though it is not be most secure way of doing things. Also it is sap rising
how relatively simple it is to provision all the boards in `grafana`.

On the other hand it is hard to inspect the data using the `grafana` interface
when compared with `kibana` where you just click through the interface and
filter data with single click.

## 2. Experiment `loki`, `garfana` and `fluentd` driver

Again the configuration is fairly straightforward but getting it into
production configuration would be quite a lot of work because of the multiple
options the `loki` has.

This setup sufferers form the same issues as **1. Experiment**. The `grafana`
interface is not as good as `kibana` and the filtering capabilities seams to
be a bit limited. Also the fact that `loki` seams to need anything searchable
to be defined as tag and it seams to be only possible to do so at the source.

## 3. Experiment `elasticsearch`, `kibana` and `fluentd` driver

Make the `elasticsearch` run and provision it at least somewhat sufficiently
for this experiment was difficult compare to other mentioned databases. Also
it seams to be the most distant configuration from actual deployment scenarios.

That said, after making everything work. This setup is quite user friendly.
Searching/exploring is simple and most of the usefully things is just few
clicks away.

## 4. Experiment `elasticsearch` and `haskell` `json` log rotate

Here again the `elasticsearch` stack is used with the difference how the data
are getting into the database. Here the log rotate it used to share the data
between the `hasekll` application and `opentelemetry-collector` sidecar.

Note: The neither the `haskell` log rotate or the `opentelemetry-collector`
are not providing the log cleanup on the shred disk.

The tricky part here was the setup of the collector's file parser and also
the mapping of the `json` fields into the `otlp` data structures.

In hindsight, it is probably better possible to us the stdout logging
and decode the data using the `json_parser` operator. Then the cleanup is
not needed and all the issues with synchronization using the file systems
goes away.

## 5. Experiment `elasticsearch`, `kibana`, `syslog` driver and json logs

The configuration is a bit easier than with log rotate because there is no
need to mount volumes and the code is also a bit simpler. The tricky part was
to figure out how the get the `opentelemetry-collector` to parse the `json`
data. Originaally the `fluentd` protocol was used but the receiver doesn't
support operator so the switch to `syslog` was needed. After the switch there
was an issue with UDP packet not being accepted by `opentelemetry-collector`
(unknown cause) but this was resolved by using TCP instead.
