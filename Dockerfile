FROM eclipse-temurin:17.0.15_6-jdk

RUN apt-get update && apt-get install -y curl unzip build-essential z3

ENV STAINLESS_VERSION=0.9.9.0
ENV DOWNLOAD_LINK=https://github.com/epfl-lara/stainless/releases/download/v${STAINLESS_VERSION}/stainless-dotty-standalone-${STAINLESS_VERSION}-linux.zip
RUN curl -L -o stainless.zip ${DOWNLOAD_LINK} && \
    unzip stainless.zip -d stainless && \
    mv stainless /opt/stainless && \
    rm stainless.zip

ENV PATH="/opt/stainless:$PATH"

WORKDIR /app
COPY . /app

ENTRYPOINT ["./entrypoint.sh"]
