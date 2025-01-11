FROM roclang/nightly-ubuntu-latest AS builder

WORKDIR /app

COPY assets assets/
COPY index.html main.roc ./

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
