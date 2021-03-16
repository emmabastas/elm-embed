> # 🚨 This tool is in early development 🚨
> Breaking changes and bugs are to be expected

# elm-embed

Read, parse and embed environment variables and file content into your Elm code.

## Core concepts

Data is embedded with _Embedders_. Embedders live inside a special folder, `elm-embed-scripts`, and they're any Elm declaration with the signature `Embed.Task`. Working with embedders is similar to working with JSON decoders, you can for example read a file with `Embed.File.read`, and parse it with `Embed.andThen`. If the parsing succeeds you can embed the value with `Embed.succeed`, and if it fails you can report why with `Embed.fail`. If all the embedders succeed then the resulting values will be generated and placed as Elm code inside `src/generated` where they can be consumed by the rest of the codebase.

## Examples

### Environment variables

When developing, you might want to use a local server instead of the production one. To achieve, this an environment variable can be embedded that contains the url of the server to use.

`elm-embed-scripts/Server.elm`:
```elm
import Embed
import Embed.Environment

server : Embed.Task String
server =
    Embed.Environment.string "server_url"
```

Running `elm-embed run` when the environment variable `server_url` is set to `localhost:8080` will give you this inside `src/generated/Server.elm`:
```elm
server : String
server =
    "localhost:8080"
```

### Markdown parsing

You might want to make a page from a markdown document instead of Elm code. The normal approach would be to include the document as a string and then parse and render it at run-time. This has several drawbacks. Parsing takes time. It's also annoying and error prone to embed markdown in an Elm string by hand. Lastly you have to deal with the case where parsing fails and give the user some sort of error page. With `elm-embed` you can read a `.md` file, parse it at build-time, and if the parsing fail's you get the error as a developer at build-time instead of as a user at run-time.

This example parses a markdown document with [dillonkearns/elm-markdown](https://package.elm-lang.org/packages/dillonkearns/elm-markdown/latest/)

```elm
-- elm-embed-scripts/Markdown.elm
import Embed
import Embed.File
import Markdown.Block exposing (Block(..), HeadingLevel(..), Inline(..))
import Markdown.Parser

document : Embed.Task (List Block)
document =
    Embed.andThen
        (\markdown ->
            case Markdown.Parser.parse markdown of
                Ok blocks ->
                    Embed.succeed blocks

                Err _ ->
                    Embed.fail "Parse error"
        )
        (Embed.File.read "markdown.md")

```

**Note:** If you're interested in this it might be worth checking out [elm-pages](https://elm-pages.com/) as well. It can do markdown parsing at build-time and a bunch of other cool stuff, batteries included. `elm-embed` is a tool for one specific thing while `elm-pages` is more of a frameworky kind of deal.

### API documentation

`elm-embed` places some modules of it's own in `elm-embed-scripts` to be used by your embedders. You can see the full API for those modules <a href="https://elm-doc-preview.netlify.app/Embed?repo=emmabastas%2Felm-embed-docs&version=master" target="_blank">Here</a>.

## Installation

### Linux

Running the code bellow will download a prepuilt binary for 64-bit linux, make it executable and place it in `/usr/local/bin`.
This works for most Linux distributions.

```bash
curl -L -o elm-embed.gz https://github.com/emmabastas/elm-embed/releases/download/0.1.0/elm-embed-0.1.0-linux-x64.gz
gunzip elm-embed.gz
chmod +x elm-embed
sudo mv elm-embed /usr/local/bin/
```

Now `elm-embed` should be callable from the command-line.

Uninstall with `sudo rm /usr/local/bin/elm-embed`

### Windows or Mac

There are currently no prebuilt binaries for Windows or Mac, the only option is to build from source, it's the same process as when building the Elm compiler.

If you send me a message in the Elm slack (@emmabastas) that would really motivate me to fix this :)

## Usage

Initialize with `elm-embed init`.

Run embedders with `elm-embed run`.

Get help with `elm-embed --help`.

## Contributing & Setting expectations

The best way to contribute right now is to provide me with use cases by leaving a comment [here](https://github.com/emmabastas/elm-embed/issues/7).
Be as detailed as possible. What could `elm-embed` do for you and how? What would need to be changed/added?

These use cases will really help me make more informed design descissions in the future, but I cannot promise that your specific use case will be implemented.
I want `elm-embed` to be the best it can within it's scope, and that means rejecting things that fall outside the scope, and also taking the time to think things through, instead of rushing features. 
