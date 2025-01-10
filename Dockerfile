FROM roclang/nightly-ubuntu-latest AS builder

WORKDIR /app

COPY assets assets/
COPY vendor vendor/
COPY index.html main.roc ./

WORKDIR vendor/kingfisher
RUN ["wget", "https://ziglang.org/download/0.11.0/zig-linux-x86_64-0.11.0.tar.xz"]
RUN ["apt" ,"install", "xz-utils"]
RUN ["tar", "-xf", "zig-linux-x86_64-0.11.0.tar.xz"]
ENV PATH="/app/vendor/kingfisher/zig-linux-x86_64-0.11.0:${PATH}"
RUN ["apt", "install", "-y", "golang"]
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

EXPOSE 9000

STOPSIGNAL SIGINT

ENTRYPOINT ["/main", "--events-file=/database/db.events"]
