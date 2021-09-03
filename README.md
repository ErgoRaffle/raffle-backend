# ErgoRaffle
This project is a crowdfunding service that aims to enable anyone to raise enough money needed for a project. The project can be a direct donation to a charity, an academic or business plan, or anything else the creator can convince people to part with their hard-earned ERG for.

## Setup Back-End Side
### Prerequisite
#### OpenJDK 8+
Download and install the appropriate [OpenJDK](https://openjdk.java.net/projects/jdk8/) binaries.
#### sbt 1.2.7
Install sbt 1.2.7 [(manual)](https://www.scala-sbt.org/1.0/docs/Setup.html)

### ErgoRaffle Front-End Side
Get the latest `raffle-frontend`
```shell
$ git submodule update --init
```  
Build the `raffle-frontend` [(instructions)](https://github.com/ErgoRaffle/raffle-frontend/blob/master/README.md)

copy the `raffle-frontend/build` into `raffle-backend/public`
```shell
$ mv raffle-frontend/build raffle-backend/public
```

## Installation
Build `raffle-backend`
```shell
$ cd raffle-backend
$ sbt assembly
```
Set the configs and update the keys. An example is available in `raffle-backend/conf/application.conf`.

Run the `raffle-backend`  

```shell
$ java -Dconfig.file=path/to/config -jar ErgoRaffle-<ERGO_RAFFLE_VERSION>.jar
```

