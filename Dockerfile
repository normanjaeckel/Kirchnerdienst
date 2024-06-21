FROM roclang/nightly-ubuntu-latest as builder

WORKDIR /app

COPY assets assets/
COPY vendor vendor/
COPY index.html main.roc ./

RUN ["roc", "build"]


FROM gcr.io/distroless/base-nossl

LABEL org.opencontainers.image.title="Kirchnerdienst"
LABEL org.opencontainers.image.description="A small tool to manage the working times of church assistants."
LABEL org.opencontainers.image.licenses="MIT"
LABEL org.opencontainers.image.source="https://github.com/normanjaeckel/Kirchnerdienst"
LABEL org.opencontainers.image.documentation="https://github.com/normanjaeckel/Kirchnerdienst/blob/main/README.md"


COPY --from=builder /app/main /

EXPOSE 8090

ENTRYPOINT ["/main"]
