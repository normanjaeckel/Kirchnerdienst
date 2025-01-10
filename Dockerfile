FROM roclang/nightly-ubuntu-latest AS builder

WORKDIR /app

COPY assets assets/
COPY vendor vendor/
COPY index.html main.roc ./

WORKDIR vendor/kingfisher
RUN ["apt", "install", "-y", "golang"]
RUN ["wget", "https://ziglang.org/download/0.11.0/zig-linux-x86_64-0.11.0.tar.xz"]
RUN ["apt" ,"install", "xz-utils"]
RUN ["tar", "-xf", "zig-linux-x86_64-0.11.0.tar.xz"]
ENV PATH="/app/vendor/kingfisher/zig-linux-x86_64-0.11.0:${PATH}"
RUN ["roc", "build.roc"]
WORKDIR /app

RUN ["roc", "build"]


FROM gcr.io/distroless/base-nossl

LABEL org.opencontainers.image.title="Kirchnerdienst"
LABEL org.opencontainers.image.description="A small tool to manage the working times of church assistants."
LABEL org.opencontainers.image.url="https://github.com/normanjaeckel/Kirchnerdienst"
LABEL org.opencontainers.image.source="https://github.com/normanjaeckel/Kirchnerdienst"
LABEL org.opencontainers.image.licenses="MIT"

COPY --from=builder /app/main /

VOLUME ["/database"]

EXPOSE 8090

STOPSIGNAL SIGINT

ENTRYPOINT ["/main", "--snapshot-file=/database/db.snapshot", "--requests-file=/database/db.requests"]
