# Markdown Parser for Roam Research 𐃏

## Rationale

Roam Research is a revolution in information technology. Although, its current parser for Roam is not great, and it can sometimes be quirky. Thus, I have set out to create an improved parser, based on ClojureScript, that addresses the problems of the current implementation in Roam.

Unlike most markdown, Roam's language can include notation of complex, recursive data structures, like code — yet it must tolerate the vague and context-dependent nature of standard markdown.

Therefore, this parser aims to provide desired parsing results and allow the user to express a variety of meanings that are currently not possible. For example, you currently cannot make an alias bold (`**[x](y)**`), have an image inside of an alias (`[![alt-text](image-url)](alias-url)`), or do this: `[[clickable text](url)]` (where the outer brackets are non-clickable).

This project is based on a challenge set out in [this tweet](https://twitter.com/Conaw/status/1334626650785341441) by Conor.

Skip to:

- [A high-level overview of this parser](#Concepts).
- [Current state of development](#There-are-still-things-on-the-to-do-list)
- [Current features](#Coverage)
- [Performance](#Performance)

## Test it

```
shadow-cljs watch dev
```
...or however you prefer to start up shadow-cljs projects.

Open a browser at http://localhost:8021/ and crack open the devtools.

- `window.block(string)` to parse a string.
- `window.help(startLine, endLine)` to benchmark against lines of the [help database](https://roamresearch.com/#/app/help/).
- `window.str(string)` to parse a string into data, and then convert that back into a string.
- `window.helpstr(startLine, endLine)` to benchmark string->data->string using the help database.

There are also various devcards to demonstrate some scenarios.

## Coverage

You can currently convert a string into a parsed data structure and then convert this back to a string.

Elements that have been implemented so far:

- :hiccup
- > block quote
- hr `---`
- code block
- `code`
- {{ }} components
- [[ ]], #[[ ]], #tag
- alias
- image
- ((parenthetical))
- ((block ref))
- $$latex$$
- formatting (bold, italic, highlight, strikethrough)
- inline-url - www.example.com, https://example.com
- attribute::

Ability to parse {{ }} components:

- Simple components like {{count}}
- query
  - Assign a name using {{query: Query title {and: [[page]]}}}
  - Reads `and`, `not`, `between`, and `or` recursively to form a map of pages and block refs
- Δ
  - Interprets a rule in the format {{Δ: 3+4}}
  - Can be extended in the future to specify options with a list eg {{Δ: 3+4 {:style snooze}}} etc
- attr-table, embed, mentions and roam/render all take a single page/block-ref
- `{{=: {{diagram}} [[visible]] | **hidden**}}`
- `{{or: #anything | ((goes)) | here}}`
- pdf, youtube, video, iframe and calc all take a text argument. The parser provides a function that allows block refs to be substituted with their text contents to end up with a complete string.

Further notes:

- The parser is capable of matching brackets correctly, not just for `[[[[nested]] pages]]` but also for things like `[[CLICK ME]](url)` (parsed as alias with `[CLICK ME]` visible) or `[[some [thing]]]` (parses as a page link to `some [thing]`). It addresses [this issue](https://github.com/Roam-Research/issues/issues/186) and [that issue](https://github.com/Roam-Research/issues/issues/17).

- Whilst guiding this project in Roam, I have been inconvenienced by the fact that I cannot express a single backtick (`) with monospace/code formatting, as escape characters are not a thing. So, this parser turns any occurence of ``` into just that (as long as it does not pair up to form a codeblock).

- I envision that in the future, S-expressions like  {:rule (if test? (inc :var) (dec :var)) } can be used in {{ }} components to provide more configuration. The parser can interpret a list of forms separated by whitespace (including commas).

## Performance

Parsing the first 1000 lines of the help database in *development mode*:
Running `window.help(0,1000)` 5 times gives

```
"Elapsed time: 417.125000 msecs"
"Elapsed time: 258.085000 msecs"
"Elapsed time: 230.050000 msecs"
"Elapsed time: 223.215000 msecs"
"Elapsed time: 219.290000 msecs"
```

Notes on performance:
- These numbers vary depending on the sample of lines (i.e. the specified line numbers). Some lines are more intense than others, and 1000 is a small sample.

- I have not focused on optimising performance yet, so I expect there will be some good gains to be had.
  - For example, the parser currently iterates through each individual character to check for actions to be done. Given that most characters are plain text, runs of non-special characters could be skipped over using regex to determine when to do this.

- Production code will be leaner and faster.

- The time it takes to construct a string from Clojure data is insignificant.

# The Code

## Concepts

Two important goals for the parser:

- Modularity and extensibility (ease of adding new elements of a variety of natures)

- Accuracy of the parse (whether it make sense and mirrors the user's intent)

I have spent a lot of time thinking about the theory of a reliable and extensive parser. This is my second attempt at building a Roam parser and I believe it meets the goals for extendability and correctness.

Here's a high-level overview of the parsing process:

- It starts with a state, and each character in the string is processed as a command that transforms the state

- When processing a character, we go through a stack of state transformers that will only take action when the conditions are right, eg when finding a `[[` to mark a page opening.

- If not transformers take action, we do nothing and move onto the next character.

- Transformers can do **anything** they want with the state, as long as they conform to certain rules -- this makes the parser very flexible as commonalities can be refactored into shared helper functions.

- "Contexts" can be created to modify the events that take place when parsing subsequent characters
  - They define contexts that are allowed as children (eg bold can contain italic -- `**__x__**` -- but code should not contain a clickable page -- `[[page surrounded by backticks]]`)
  - Contexts define contexts that they are 'killed by'. For instance, given `**{{a}}**`, the curly brackets component should take priority and nothing should be bolded

This parser is very much a believer in being decisive, making assumptions, and correcting errors if necessary. It is not a perfectionist.

<blockquote class="twitter-tweet"><p lang="en" dir="ltr">Anything worth doing is worth doing wrong.<br><br>Only way to start.</p>&mdash; Conor White-Sullivan 𐃏🇺🇸 (@Conaw) <a href="https://twitter.com/Conaw/status/1318712092132745216?ref_src=twsrc%5Etfw">October 21, 2020</a></blockquote>

In concrete terms, if the parser reaches a decision point in the string that could mean multiple things (eg "["), it chooses just one state transformation and moves on. This quality makes it easy to think about the parsing process when writing extensions.

Later down the line, it could be proven that a certain state transformation was the wrong life decision (eg no matching "]" was found or there was a conflict with regard to the 'killed by' rules). In this case, we can simply look to the relevant 'context' which defines instructions for backtracking to the decision point and resuming with an alternative option -- then fingers crossed to see if that works out. All praise the immutable data in Clojure!

While this method does seem inefficient, my countless hours of 'floor time' (I don't have a hammock) have lead me to believe that this work is unavoidable if you want a robust parser that respects your rules whilst offering convenient modularity.

## There are still things on the to-do list

Here are some examples of things that have not been done as of yet:

- Turn up the performance -- eg identify tokens of interest
- Refactor individual components out of render.cljs -- clean that file up.
- Further refactoring to capture commonalities between state transform rules.
- Escape characters

Considering this task from the original brief:

> Provide an abstraction for Replacing each item in the tree with something else (like an html component, or a new string value)

I have not directly addressed this requirement since I do not have the full details of what is needed of the API and how it will be used. That being said, the parsed data is provided as friendly Clojure maps and vectors, so if you knew what you wanted to change, you could leverage the built-in Clojure facilities like `update-in`. If necessary, more specialised functions could easily be built on these. Then, if you wanted the updated string, you can use the associated `stringify` function for the modified element. Though, since stringifying is super fast compared to parsing, you might as well stringify the entire block string, which ensures everything is stringified properly.

## History

Originally, I had designed a parser that extracted all tokens from the string using regex. Then, it would pass over each type of token separately and match pairs of delimiters. There would be a recursive process of repeating the parse for the children of each pair. While I had this design down to some good performance numbers (30–160ms per 1000 lines), I felt it got quite complex and had less flexibility.

See my [Notion publication for V1](https://www.notion.so/crypticbutter/Roam-Parser-V1-acd4a5a5aad34a9f849e5c2477f505a3).
