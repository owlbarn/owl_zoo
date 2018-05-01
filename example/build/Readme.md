# Example 1

This example shows how to build a Docker container of the [SqueezNet](https://gist.github.com/jzstark/aa36ee2c93fad476f4a46dc195b6fd89) image classification service.
The container provides two interfaces: `/predict/infer` and `/predict/to_json`. The former returns image classification result as a `1x1000` ndarray, and the latter interprets this ndarray into top-5 inference result in JSON format.

Here is the scripts to build the container:

```ocaml
open Owl
open Owl_newt
open Owl_zoo_build

let gist = "aa36ee2c93fad476f4a46dc195b6fd89" in
let backend = CREST {dname = "alice/squeeznet:latest"} in
build backend gist
```

After than, you need to start the container:
```
docker run --name sqnet -p 9527:9527 -it alice/squeeznet:latest
```

Then send requests to the docker. For example, you can use the provided Python script:
```
python request.py
```

And you can see the classification result:
```
[{"class":"giant panda, panda, panda bear, coon bear, Ailuropoda melanoleuca", "prop": 0.997042715549},{"class":"earthstar", "prop": 0.00132688984741},{"class":"indri, indris, Indri indri, Indri brevicaudatus", "prop": 0.000427583931014},{"class":"Bedlington terrier", "prop": 0.000151671396452},{"class":"Chihuahua", "prop": 0.000112503417768} ]

```

Note that we send a request to one endpoint, get the result, and send it to the other. We'll show how to compose these services before building the container so that only one request need to be sent.
