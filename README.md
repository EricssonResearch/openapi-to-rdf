# KB and Reasoner for yaml-to-rdf

## Option 1. Build from source and run

```
docker build --build-arg USER=<USERNAME> --build-arg TOKEN=<TOKEN> -t knowledgebase .
docker run -i -p 3020:3020 -t knowledgebase
```

where `<USERNAME>` is your GitLab user name and `<TOKEN>` can be created [[here](https://gitlab.internal.ericsson.com/-/profile/personal_access_tokens)].

## Option 2. Setup local development environment

1. Install SWI-Prolog
   * Option 1: Follow standard installation procedure to install _development release_ (https://www.swi-prolog.org/download/devel).
   * Option 2 (MacOS): install Leo's homebrew package:
   ```
   brew install likelion/tap/swipl
   ```
2. Create main folder in your preferred location (`/opt/kb` in this example)
   ```
   mkdir -p /opt/kb
   cd /opt/kb
   ```
3. Clone ClioPatria application server
   ```
   git clone -b 'V3.1.1' https://github.com/ClioPatria/ClioPatria.git
   ```
4. Create runtime folder and configure ClioPatria server
   ```
   mkdir run
   cd run
   ../ClioPatria/configure
   ```
   Try to start the server
   ```
   ./run.pl
   ```
   If it does not start, replace the first line in `/opt/kb/run.pl` with this one:
   ```
   #!/usr/bin/env swipl
   ```
5. Clone required cpacks
   ```
   mkdir cpack
   cd cpack
   git clone https://gitlab.internal.ericsson.com/cognitive-layer/reasoner.git
   git clone https://gitlab.internal.ericsson.com/er-brazil/yaml-to-rdf.git
   mv yaml-to-rdf/yaml_importer .
   rm -rf yaml-to-rdf/
   ```
6. Copy settings, start the server, configure cpacks
   ```
   cp cognitive_core/*.db ..
   cd ..
   ./run.pl
   ?- cpack_configure(reasoner), cpack_configure(yaml_importer).
   ?- halt.
   ```
7. Start the server normally
   ```
   ./run.pl
   ```
   Access UI at `http://localhost:3020`. Default `admin` password is `s3cret`.

