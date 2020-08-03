ARG GHC_V=8.8.3
FROM andreus108/haskell:${GHC_V}
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

CMD ["stack", "exec", "--resolver", "ghc-8.6.4", "playground-exe"]
