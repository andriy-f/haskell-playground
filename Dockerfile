# Reminder: if you change version, change CMD accordingly
ARG GHC_V=9.4.8
ARG STACK_RESOLVER=lts-21.25
FROM haskell:${GHC_V}
ARG GHC_V
ARG STACK_RESOLVER

WORKDIR /app

COPY package.yaml ./
COPY stack.yaml stack.yaml.lock ./

# Install dependencies
RUN stack setup --resolver $STACK_RESOLVER

COPY app ./app
COPY src ./src
COPY test ./test
COPY Setup.hs README.md ChangeLog.md ./

# Build
RUN stack build --resolver $STACK_RESOLVER

ENV GHC_V=${GHC_V}
CMD exec stack exec --resolver $STACK_RESOLVER playground-exe
