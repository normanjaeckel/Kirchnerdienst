# Kirchnerdienst

A small tool to manage the working times of church assistants of our parish.

Clone this repository including submodules:

    git clone --recurse-submodules https://github.com/normanjaeckel/Kirchnerdienst.git

Get [Roc](https://www.roc-lang.org/) and then just run

    roc build

to build the binary.

Run

    ./main --help

to see the available options.


## Docker image

You can use the Docker image for example like this:

First create a custom volume:

    docker volume create my-volume

Then run the container and publish the container's port onto an ephermeral port
on the host:

    docker run --detach \
        --name my-app \
        --rm \
        --mount source=my-volume,target=/database
        --publish-all \
        ghcr.io/normanjaeckel/kirchnerdienst

List containers to see port that was chosen:

    docker container ls


## License

[MIT](LICENSE)


## Credits

We use the JS libraries [htmx](https://htmx.org) and
[_hyperscript](https://hyperscript.org). If you build the binary of this
project, htmx' minified JS file and _hyperscript's minified JS file are
included. htmx is released under the [Zero-Clause BSD
license](assets/htmx/LICENSE). _hyperscript is released under the [BSD 2-Clause
License](Server/assets/_hyperscript/LICENSE).
