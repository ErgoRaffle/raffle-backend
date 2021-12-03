FROM mozilla/sbt:8u181_1.2.7 as builder
COPY ./app /raffle/app/
COPY ./conf /raffle/conf/
COPY ./project /raffle/project/
COPY ./test /raffle/test/
COPY ./build.sbt /raffle/
WORKDIR /raffle
RUN sbt assembly
RUN mv `find . -name ErgoRaffle-*.jar` /ergo-raffle.jar
CMD ["java", "-jar", "/ergo-raffle.jar"]

FROM openjdk:8-jre-slim
RUN adduser --disabled-password --home /home/ergo/ --uid 9052 --gecos "ErgoPlatform" ergo && \
    install -m 0750 -o ergo -g ergo  -d /home/ergo/raffle
COPY --from=builder /ergo-raffle.jar /home/ergo/ergo-raffle.jar
COPY ./conf/application.conf /home/ergo/raffle/application.conf
RUN chown ergo:ergo /home/ergo/ergo-raffle.jar
USER ergo
EXPOSE 9000
WORKDIR /home/ergo
ENTRYPOINT java -jar -D"config.file"=raffle/application.conf /home/ergo/ergo-raffle.jar
