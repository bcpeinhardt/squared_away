# Squared Away

### A statically typed spreadsheet with testing as a first class citizen

This project is really really new. This repo contains a prototype
spreadsheet in Gleam implemented as a Lustre app, which will allow 
me to quickly iterate on the formula language and the spreadsheet UX.

The intention is to get a good idea of what I actually want an MVP
to look like, then do a rewrite as a native app using a lower level 
language.

### Goals of this project

1. Make spreadsheet formulas actually readable (have a formula language that optimizes for readability).
2. Produce friendly errors that guide the user to fix bugs in their spreadsheets.
3. Prioritize testing as a first class citizen.
4. Use a combination of static typing and test coverage to make refactoring spreadsheets not scary.
5. Provide useful primitives for common tasks.
6. Be fast enough for real world use cases, even for driving services off of spreadsheets (that's a less crazy thing to do when they're statically typed and well tested).
7. Provide a diff based contribution workflow for teams contributing to the same spreadsheets.

### Contributing
There's not a hell of a lot in the way of docs or tests yet, I'm very much still "prototyping".
If you're interested in contributing and have questions, feel free to file an issue.
