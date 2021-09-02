FROM node:12.14 as builder-front

WORKDIR /usr/src/app
COPY ./raffle-frontend/package.json ./raffle-frontend/package-lock.json ./
RUN npm install
COPY ./raffle-frontend ./
RUN npm run build

FROM mozilla/sbt:8u181_1.2.7 as builder

ADD ["./raffle-backend", "/raffle/raffle-backend"]
WORKDIR /raffle/raffle-backend
COPY --from=builder-front /usr/src/app/build/ ./public/
RUN sbt assembly
RUN mv `find . -name ErgoRaffle-*.jar` /ergo-raffle.jar
CMD ["java", "-jar", "/ergo-raffle.jar"]

FROM openjdk:8-jre-slim
RUN adduser --disabled-password --home /home/ergo/ --uid 9052 --gecos "ErgoPlatform" ergo && \
    install -m 0750 -o ergo -g ergo  -d /home/ergo/raffle
COPY --from=builder /ergo-raffle.jar /home/ergo/ergo-raffle.jar
COPY ./raffle-backend/conf/application.conf /home/ergo/raffle/application.conf
RUN chown ergo:ergo /home/ergo/ergo-raffle.jar
USER ergo
EXPOSE 9000
WORKDIR /home/ergo
ENTRYPOINT java -jar -D"config.file"=raffle/application.conf /home/ergo/ergo-raffle.jar
