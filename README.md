# `elm-embed`

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

You might want to make a page from a markdown document instead of Elm code. The normal approach would be to include the document as a string and then parse and render it at run-time. This has several drawbacks. Parsing takes time. It is also annoying and error prone to embed markdown in an Elm string by hand. Lastly you have to deal with the case where parsing fails and give the user some sort of error page. With `elm-embed` you can read a `.md` file, parse it at build-time, and if the parsing fail's you get the error as a developer at build-time instead of as a user at run-time.

This example parses markdown document with [dillonkearns/elm-markdown](https://package.elm-lang.org/packages/dillonkearns/elm-markdown/latest/)

```elm
-- elm-embed-scripts/Markdown.elm
import Embed
import Embed.File
import Markdown.Block exposing (Block(..))
import Markdown.Parser

document : Embed.Task Block
document =
    Embed.andThen
        (\markdown ->
            case Markdown.Parser.parse markdown of
                Ok blocks ->
                    Embed.succeed blocks

                Err error ->
                    Embed.fail "Parse error"
        )
    (File.read "blog/post1.md")
```

**Note:** If you're interested in this it might be worth checking out [elm-pages](https://elm-pages.com/) as well. It can do markdown parsing at build-time and a bunch of other cool stuff, batteries included. `elm-embed` is a tool for one specific thing while `elm-pages` is more of a frameworky kind of deal.

### API doccumentation

`elm-embed` places some modules of it's own in `elm-embed-scripts` to be used by your embedders. You can see the full API for those modules <a href="https://elm-doc-preview.netlify.app/Embed?repo=emmabastas%2Felm-embed-docs&version=master" target="_blank">Here.</a>

## Installation

## Usage

Initialize with `elm-embed init`.

Run embedders with `elm-embed run`.

Get help with `elm-embed --help`.

## License
