# zlox

A Zig port of the tree-walk interpreter from Crafting Interpreters book.

It's slower than the Java version, and leaks quite a bunch of memory due to me
being lazy and using an arena to avoid having to free stuff.
