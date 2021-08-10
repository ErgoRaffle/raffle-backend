FROM node:12.14 as builder-front

WORKDIR /usr/src/app
COPY ./raffle-front-end/package.json ./raffle-front-end/package-lock.json ./
RUN npm install
COPY ./raffle-front-end ./
RUN npm run build

FROM openjdk:8-jre-slim as builder
ENV DEBIAN_FRONTEND noninteractive
RUN apt-get update && \
    apt-get install -y --no-install-recommends apt-transport-https apt-utils bc dirmngr gnupg && \
    echo "deb https://repo.scala-sbt.org/scalasbt/debian all main" | tee /etc/apt/sources.list.d/sbt.list && \
    echo "deb https://repo.scala-sbt.org/scalasbt/debian /" | tee /etc/apt/sources.list.d/sbt_old.list && \
    apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv 2EE0EA64E40A89B84B2DF73499E82A75642AC823 && \
    apt-get update && \
    apt-get upgrade -y && \
    apt-get install -y --no-install-recommends sbt wget
WORKDIR /raffle
RUN wget https://github.com/graalvm/graalvm-ce-builds/releases/download/vm-21.1.0/graalvm-ce-java8-linux-amd64-21.1.0.tar.gz && \
    tar -xf graalvm-ce-java8-linux-amd64-21.1.0.tar.gz
ENV JAVA_HOME="/raffle/graalvm-ce-java8-21.1.0"
ENV PATH="${JAVA_HOME}/bin:$PATH"
ADD ["./appkit/", "/raffle/appkit"]
WORKDIR /raffle/appkit
RUN sbt publishLocal
ADD ["./raffle-back-end", "/raffle/raffle-back-end"]
WORKDIR /raffle/raffle-back-end
COPY --from=builder-front /usr/src/app/build/ ./public/
RUN sbt assembly
RUN mv `find . -name getinplay_2.12-*.jar` /ergo-raffle.jar
CMD ["java", "-jar", "/ergo-raffle.jar"]

FROM openjdk:8-jre-slim
RUN adduser --disabled-password --home /home/ergo/ --uid 9052 --gecos "ErgoPlatform" ergo && \
    install -m 0750 -o ergo -g ergo  -d /home/ergo/raffle
COPY --from=builder /ergo-raffle.jar /home/ergo/ergo-raffle.jar
COPY ./raffle-back-end/conf/application.conf /home/ergo/raffle/application.conf
RUN chown ergo:ergo /home/ergo/ergo-raffle.jar
USER ergo
EXPOSE 9000
WORKDIR /home/ergo
ENTRYPOINT java -jar -D"config.file"=raffle/application.conf /home/ergo/ergo-raffle.jar
