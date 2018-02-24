# Hacking Java on Emacs

I sort of started pro-emacs when Java came out. There was JDE (which
is still around) but it annoyed the hell out of me so I implemented a
bunch of little things to help.

I got out of Java in the early 2000s but working in an Enterprise it's
hard to avoid. I just refuse to use any of these IDE things. I'd
rather not use Java than use one of those.

So here's my, probably growing, collection of things that I use to
wrangle Java, some of them rescued from 20 years ago when Java
started.

## When in a Java file

```
key             binding
---             -------
C-c #           nj-open-shell
C-c .           nj-open-project
C-c 4           Prefix Command
C-c f           nj-open-file-in-project
```

`nj-open-project` opens a project view with the java files listed.

`nj-open-shell` opens a shell in the top level of your project.

## When in the project view

```
key             binding
---             -------
RET             nj-list-open
q               nj-list-quit
```

`nj-list-open` opens a Java file from the list.

### What's a project?

Something with a pom.xml file.

### How are Java files found?

Emacs uses it's `find-program`. This may not be available, on Windows
for example.
