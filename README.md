# ErgoRaffle
This project is a crowdfunding service that aims to enable anyone to raise enough money needed for a project. The project can be a direct donation to a charity, an academic or business plan, or anything else the creator can convince people to part with their hard-earned ERG for.

## Setup Back-End Side
### Prerequisite
#### OpenJDK 8+
Download and install the appropriate [OpenJDK](https://openjdk.java.net/projects/jdk8/) binaries.
#### sbt 1.2.7
Install sbt 1.2.7 [(manual)](https://www.scala-sbt.org/1.0/docs/Setup.html)

## Installation
Build `raffle-backend`:
```shell
$ clone https://github.com/ErgoRaffle/raffle-backend.git
$ cd raffle-backend
$ sbt assembly
```
Set the configs and update the keys. An example is available [(here)](./conf/application.conf) .

Run the `raffle-backend`

```shell
$ java -Dconfig.file=path/to/config -jar ErgoRaffle-<ERGO_RAFFLE_VERSION>.jar
```

Also the `raffle-frontend` instructions is [(here)](https://github.com/ErgoRaffle/raffle-frontend/blob/master/README.md)

## Docker quick start
To use docker, follow instructions:
Build a docker image:
```shell
docker build -t raffle-backend:latest .
```
Then run the raffle-backend with the help of the built docker image:
```shell
$ mkdir /desired/path/db
$ chown -R 9052:9052 /desired/path/db
$ docker run -p 8080:8080 \
  --restart=always \
  -v /desired/path/configFile.conf:/home/ergo/raffle/application.conf \
  -v /desired/path/db:/home/ergo/data/ \
  -d raffle-backend:latest
```
The raffle is up and running on port 8080.
