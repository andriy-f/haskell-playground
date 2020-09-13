# Reminder: if you change version, change CMD accordingly
ARG GHC_V=8.8.4
FROM haskell:${GHC_V}
ARG GHC_V

WORKDIR /app

COPY package.yaml ./
COPY stack.yaml stack.yaml.lock ./

# Install dependencies
RUN stack setup --resolver ghc-$GHC_V

COPY app ./app
COPY src ./src
COPY test ./test
COPY Setup.hs README.md ChangeLog.md ./

# Build
RUN stack build --resolver ghc-$GHC_V

ENV GHC_V=${GHC_V}
CMD exec stack exec --resolver ghc-$GHC_V playground-exe
