ARG GHC_V=8.6.4
FROM andreus108/haskell:${GHC_V}
ARG GHC_V

WORKDIR /app

COPY stack.yaml ./
COPY package.yaml ./

# Install dependencies
RUN stack setup --resolver ghc-$GHC_V

COPY app ./app
COPY src ./src
COPY test ./test
COPY Setup.hs ./

# Build
RUN stack build --resolver ghc-$GHC_V

CMD ["stack", "exec", "--resolver", "ghc-8.6.4", "playground-exe"]
