# Author-aware features for org-change

## Motivation

org-change already carries authorship: `org-change-author` stamps new
changes with an id, `org-change-authors` maps each id to a name and a
color, changes are fontified in their author's color, and LaTeX export
emits `\definechangesauthor` lines. But nothing *acts* on authorship
interactively. You cannot set the author without `setq`/customize, the
overview does not say who made a change, and there is no way to review
or accept one collaborator's changes apart from another's.

This spec adds three things on top of the existing data model, with no
change to the markup or file format:

1. `org-change-set-author` — set the current author interactively.
2. An author column and an author filter in the overview.
3. `org-change-accept-by-author` / `org-change-reject-by-author`.

## Background: where authorship lives

A change is `{!new!}{!old!}` with an optional third `{!comment!}` group.
The author is the `@ID` prefix of that comment, parsed by the existing
`org-change--split-comment`, which returns a `(AUTHOR . TEXT)` cons:
`AUTHOR` is the id string, or nil when the comment carries no `@ID`. A
change with no comment, or a comment without an `@ID`, is *unattributed*
(author nil). This is already how the on-screen comment and the LaTeX
export decide the author, so the feature reads authorship the same way
the rest of the package does. No new markup is introduced.

## Component 1: shared helpers

Two internal helpers underpin everything else.

### `org-change--change-author`

Return the author id of the change just matched, or nil when it is
unattributed. Called with the match data of `org-change--regexp` set
(i.e. right after `org-change--search-forward` or `org-change--at-change`),
it decodes the comment group and takes the car of
`org-change--split-comment`. It must save the match data, since
`string-match` inside `--split-comment` would otherwise clobber it for
a caller that is mid-walk.

### `org-change--authors-present`

Walk the whole buffer and return a cons `(IDS . UNATTRIBUTED)`: `IDS`
is the list of distinct author ids that actually occur, in first-seen
order; `UNATTRIBUTED` is non-nil when at least one change has no author.
Empty changes (`{!!}{!!}`) are skipped, as they are everywhere else.
This is the single source for every author picker, so no picker ever
offers an author who is absent from the buffer, nor omits one who is
present but missing from `org-change-authors`.

## Component 2: `org-change-set-author`

An interactive command, not bound to a key by default — setting the
author is a session-setup action, the interactive form of what you used
to do with `setq` or customize.

It reads with `completing-read` over the entries of `org-change-authors`,
each shown as `id  (name)`, together with a `No author` entry.
`require-match` is nil, so:

- Choosing an existing entry sets `org-change-author` to that id.
- Typing an id that is not in the list sets `org-change-author` to that
  id as-is. It is used verbatim; it simply has no color until the user
  adds it to `org-change-authors`. No prompt for name or color.
- Choosing `No author` sets `org-change-author` to nil.

The command reports the result in the echo area (`Author set to SG` /
`Author cleared`).

## Component 3: overview author column and filter

### Entry shape

Overview entries grow from `(MARKER LINE SUMMARY)` to
`(MARKER LINE SUMMARY AUTHOR)`, where `AUTHOR` is the id string or nil.
`org-change--overview-entries` fills the new slot from
`org-change--change-author`.

### Author column

Each rendered line shows, between the line number and the summary, the
author id, propertized with that author's face so it carries the same
color as in the text. Unattributed changes leave the column blank. The
column is as wide as the longest id present (computed once per render),
so the summaries stay aligned:

```
  12  SG  new wording
  14  MR  ✗dropped clause
  18      an unattributed edit
```

### Filter

A buffer-local variable in the overview buffer,
`org-change-overview--filter`, holds one of:

- an author id string — show only that author's changes;
- the symbol `unattributed` — show only changes with no author;
- nil — show every change (the default).

`a` (`org-change-overview-filter`) prompts with `completing-read` over
the authors present (from `org-change--authors-present`), each shown as
`id  (name)` as in `set-author`, offering `(unattributed)` only when
unattributed changes exist, and always an `All` entry that clears the
filter. It sets the variable and re-renders.

`org-change-overview--render` filters the entry list against the
variable before drawing. A header line (`header-line-format`) shows the
active filter: `Author: SG`, `Author: (unattributed)`, or `Author: all`.

Because filtering happens in the render, the existing keys need no
change: `RET`, `k` (`org-change-overview-accept`), `x`
(`org-change-overview-reject`), and `g` all operate on the visible list,
so accepting or rejecting while filtered acts only within the chosen
author. `org-change--overview-update` — the refresh triggered when a
change is accepted or rejected in the text buffer — must read and keep
the existing filter rather than resetting it.

## Component 4: accept / reject by author

`org-change-accept-by-author` and `org-change-reject-by-author`, not
bound to keys — deliberate, occasional actions reached by name.

Each:

1. Reads an author with `completing-read` over the authors present,
   each shown as `id  (name)` as in `set-author`, offering
   `(unattributed)` when applicable. (No `All` entry: a global
   accept/reject already exists via region or the sweep.)
2. Scopes to the active region if there is one, otherwise the whole
   buffer.
3. Counts the changes in scope whose `org-change--change-author`
   matches the chosen author (or that are unattributed, for the
   `(unattributed)` choice).
4. Confirms with a `y-or-n-p`: `Accept 7 changes by SG?`, or
   `Accept 7 unattributed changes?`. Declining leaves the buffer
   untouched.
5. On acceptance, walks the scope and applies each matching change,
   then reports `7 changes accepted` / `... rejected`.

The walk reuses the marker-based machinery of `org-change--apply-region`
(markers for the moving bound, stepping past each replacement) with an
author predicate added, and calls `org-change--apply-change` per change.
The fold handling, whitespace joining, and overview refresh therefore
come for free, exactly as they do for region accept/reject.

## Interaction model

The text buffer stays modeless: `n`/`p` and the plain accept/reject keys
are unchanged and never gain hidden per-author state. The one stateful
thing — the author filter — lives in the overview, which is already a
view with its own buffer-local state. This keeps "why did `n` skip that
change?" from ever happening, while still letting a reviewer focus on
one collaborator through the overview or the by-author commands.

## Testing

ERT tests in batch, following the existing suite's style.

Shared helpers:
- `org-change--change-author` on an attributed, an unattributed, and a
  no-comment change; and that it does not disturb match data for a
  caller mid-walk.
- `org-change--authors-present` on a buffer mixing two authors and an
  unattributed change: correct ids, first-seen order, unattributed flag;
  and the empty-change skip.

`set-author` (stubbing `completing-read`):
- picking an existing id, typing a new id, and choosing `No author`.

Overview:
- the author column renders ids and leaves unattributed rows blank;
- filtering to an id, to `unattributed`, and back to all shows the right
  lines and header;
- accepting on a filtered list acts on the right change in the source;
- the filter survives a refresh triggered from the text buffer.

By-author (stubbing `completing-read` and `y-or-n-p`):
- accept and reject, region-scoped and whole-buffer, apply to exactly
  the matching changes and report the count;
- the `(unattributed)` choice targets the un-tagged changes;
- a declined confirmation leaves the buffer untouched.

## Out of scope

- No change to the markup or file format.
- No persistent buffer-local filter over the text buffer; no prefix-arg
  author variants of `n`/`p` or accept/reject. (Considered and set
  aside in favor of the overview-centered model.)
- No prompt to add a freshly typed author id to `org-change-authors`,
  with a name and color. The id is used bare; giving it a color stays a
  customize action.
- No default key bindings for `set-author` or the by-author commands.
