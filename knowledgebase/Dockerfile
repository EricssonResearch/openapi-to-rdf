FROM ubuntu:20.04 as build

LABEL maintainer "leonid.mokrushin@ericsson.com"

# Your GitLab username
ARG USER
# Your GitLab token from https://gitlab.internal.ericsson.com/-/profile/personal_access_tokens
ARG TOKEN

# Deploy arguments
ARG PUBLIC_HOST=localhost
ARG PUBLIC_PORT=3020

COPY users.db /opt/server/
COPY psettings.db /opt/server/settings.db

WORKDIR /opt

RUN apt-get update && \
    DEBIAN_FRONTEND="noninteractive" \
    apt-get -yq --no-install-recommends install tzdata software-properties-common git curl && \
    apt-add-repository ppa:swi-prolog/devel && \
    apt-get update && \
    apt-get -yq --no-install-recommends install swi-prolog && \
    git clone -b 'V3.1.1' --depth 1 https://github.com/ClioPatria/ClioPatria.git && \
    curl https://pki.ericsson.se/CertData/EGADIssuingCA3.crt --output /usr/share/ca-certificates/EGADIssuingCA3.crt && \
    curl https://pki.ericsson.se/CertData/EGADRootCA.crt --output /usr/share/ca-certificates/EGADRootCA.crt && \
    echo EGADIssuingCA3.crt >> /etc/ca-certificates.conf && \
    echo EGADRootCA.crt >> /etc/ca-certificates.conf && \
    update-ca-certificates && \
    mkdir -p server/cpack && \
    cd server/cpack && \
    git clone --depth 1 https://$USER:$TOKEN@gitlab.internal.ericsson.com/cognitive-layer/reasoner.git && \
    cd .. && \
    apt-get -y remove --purge software-properties-common git curl && \
    apt-get autoremove -y && \
    rm -rf /var/lib/apt/lists/* && \
    sh ../ClioPatria/configure && \
    sed -i 's|%PUBLIC_HOST%|'$PUBLIC_HOST'|g' settings.db && \
    sed -i 's/%PUBLIC_PORT%/'$PUBLIC_PORT'/g' settings.db && \
    swipl run.pl --after_load='cpack_configure(reasoner), halt'

WORKDIR /opt/server

EXPOSE ${PUBLIC_PORT}

CMD ["swipl","run.pl"]
