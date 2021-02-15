FROM openjdk:11 as builder

RUN curl -L https://github.com/lihaoyi/mill/releases/download/0.9.5/0.9.5 > /usr/local/bin/mill && chmod +x /usr/local/bin/mill

RUN mkdir /src
WORKDIR /src

COPY build.sc .
RUN mill client.resolvedIvyDeps
RUN mill client.scalaCompilerClasspath

COPY . .
RUN mill client.assembly


FROM openjdk:11-jre-slim

EXPOSE 8080
RUN mkdir /pack-content
VOLUME /pack-content

COPY --from=builder /src/out/client/assembly/dest/out.jar /app.jar

ENTRYPOINT ["java", "-cp", "/app.jar", "zodo.jeopardy.client.Entrypoint"]
