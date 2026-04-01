# The RT Literal Type project

The RT Literal Type project solves a security and reliability problem that has plagued computing since the shift to reserved-character tokenizers: the literal string. It accomplishes this by bringing back the Fortran H literal in a modern form, introducing editor tools to make them easy to add to source code, and presenting modified versions of compilers and interpreters so they can be parsed.

The current demonstration version introduces `RTL.el` for Emacs, and a modified NodeJS interpreter to accept the extent literal tokens.

## Problem with the current approach

The current approach to string literals used in computing relies on what communication theory calls 'in-band signaling'. Upon reaching an opening quote, the tokenizer is plunged into the text of another language, one it was probably not explicitly designed for. It then struggles to find special sequences in that stream to recover control and get back to the language it was designed for. 

When any symbol could be part of the data stream, it is impossible to guarantee that control can be recovered. Hence, conventional approaches to this problem remove a symbol from the available data pool and make it special. This is the so-called 'escape symbol'. To allow the escape symbol itself to be transmitted, a specific escape sequence is defined for it.

Escaped sequences grow exponentially in length with self-referencing or even complex structures of co-referencing, possibly heterogeneously depending on the mix of languages. In merely one or two levels, string coding becomes a riddle.

1. "Bob said, \"Alice\""
2. Line 1 text: "\"Bob said, \\\"Alice\\\"\""
3. Line 2 text: "\"\\\"Bob said, \\\\\\\"Alice\\\\\\\"\\\"\""

If a person continues the progression from line 1 to 3, the string grows exponentially. Not only will it become truly large, but no human will be able to keep track of all the escapes.

## The extent literal solution

The method of extent literals never gives up control, so there is no step for trying to recover it. With an extent literal, the payload data is written exactly as it is from its source. There is no need for preprocessing and adulteration of the data.

This works by having the programmer tell the editor when they start and finish entering a literal. The editor then drops the modified H literal encoding into the document. In the Emacs e-lisp reference implementation, a person accomplishes this by including `RTL.el` in the Emacs startup file, which provides the following commands:

All commands are routed through the `M-o` prefix:
* `M-o m` : Make a new literal.
* `M-o e` : Edit an existing literal.
* `M-o s` : Select and cycle through nested literals.
* `M-o x` : Exit, calculate the new rightmost byte index, and make the display version read only.
* `M-o a` : Abort the active edit and destroy the literal boundary.

The literal editors displays inlne with the source code, and exits automatically if the cursor leaves the literal box.

The resulting literal becomes a distinct object to interact with in the editor. Extent literals can be nested without the need for any special encoding. A programmer calls the make function while typing a literal, and the new literal becomes a nested literal within the parent literal.

## The extent literal form

A programmer should avoid manually entering the extent literal into the document. Although theoretically possible, using an RTL capable editor is highly recommended.

In the source document, the extent literal displays with soft background highlighting as:

`“”` | `“<extent> <content>”`

The first form, two matched quotes, represents a null literal. The second form has an extent number followed by a space, followed by the literal contents. When using the reference extension for Emacs, this display form is read-only. In future versions, the extent field will be hidden. Consequently, all that the user will see is a quoted literal. And yes, those are unicode left and right quotes. The tool inserts those. The user does not type them.

* Example: `“6 golfing”`
* Example of nested literals: `“C A “2 efg””`
* Example of quotes in quotes: `“3E Alice said to Bob, “1A The weather on Tuesday was good””`

Repeating the example from the escape character section:

1. Raw text: Bob said, "Alice"
2. Line 1 text: `“10 Bob said, "Alice"”`
3. Line 2 text: `“18 “10 Bob said, "Alice"””`

If a person continues the progression from line 1 to 3, the string grows linearly, not exponentially. In the editor these are made with cut and paste one line to the next; no one types the extent fields directly.

## The Advantage Over Auto-Escaping Tools

Someone could argue that if a tool is involved, the tool could simply insert the string with conventional escapes. Indeed, many people are now relying on IDEs to do exactly this. For that person, the RT Literal Type project should be a welcome refinement. It is an approach that makes such tools more reliable, and makes their output smaller and far more legible.

## Project Structure

This project utilizes the Harmony directory skeleton. 

To view the complete project documentation with its intended formatting, clone the RT style repository side-by-side with this project, and link it into the third-party directory:

```bash
cd shared/third_party
ln -s ../../../RT-style-JS_public RT-style-JS_public

Then, for example, open document/extent_literal.html in a browser, etc.
