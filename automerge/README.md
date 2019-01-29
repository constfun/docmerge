### Conflicting changes

Automerge allows different nodes to independently make arbitrary changes to their respective copies
of a document. In most cases, those changes can be combined without any trouble. For example, if
users modify two different objects, or two different properties in the same object, then it is
straightforward to combine those changes.

If users concurrently insert or delete items in a list (or characters in a text document), Automerge
preserves all the insertions and deletions. If two users concurrently insert at the same position,
Automerge will arbitrarily place one of the insertions first and the other second, while ensuring
that the final order is the same on all nodes.

The only case Automerge cannot handle automatically, because there is no well-defined resolution,
is when users concurrently update the same property in the same object (or, similarly, the same
index in the same list). In this case, Automerge arbitrarily picks one of the concurrently written
values as the "winner":

```js
let doc1 = Automerge.change(Automerge.init(), doc => { doc.x = 1 })
let doc2 = Automerge.change(Automerge.init(), doc => { doc.x = 2 })
doc1 = Automerge.merge(doc1, doc2)
doc2 = Automerge.merge(doc2, doc1)
// Now, doc1 might be either {x: 1} or {x: 2} -- the choice is random.
// However, doc2 will be the same, whichever value is chosen as winner.
```

Although only one of the concurrently written values shows up in the object, the other values are
not lost. They are merely relegated to a `_https://github.com/constfun` object:

```js
doc1 // {x: 2}
doc2 // {x: 2}
doc1._conflicts // {x: {'0506162a-ac6e-4567-bc16-a12618b71940': 1}}
doc2._conflicts // {x: {'0506162a-ac6e-4567-bc16-a12618b71940': 1}}
```

Here, the `_conflicts` object contains the property `x`, which matches the name of the property
on which the concurrent assignments happened. The nested key `0506162a-ac6e-4567-bc16-a12618b71940`
is the actor ID that performed the assignment, and the associated value is the value it assigned
to the property `x`. You might use the information in the `_conflicts` object to show the conflict
in the user interface.

The next time you assign to a conflicting property, the conflict is automatically considered to
be resolved, and the property disappears from the `_conflicts` object.

### Undo and Redo

Automerge makes it easy to support an undo/redo feature in your application. Note that undo is
a somewhat tricky concept in a collaborative application! Here, "undo" is taken as meaning "what the
user expects to happen when they hit Ctrl-Z/Cmd-Z". In particular, the undo feature undoes the most
recent change *by the local user*; it cannot currently be used to revert changes made by other
users.

Moreover, undo is not the same as jumping back to a previous version of a document; see
[the next section](#examining-document-history) on how to examine document history. Undo works by
applying the inverse operation of the local user's most recent change, and redo works by applying
the inverse of the inverse. Both undo and redo create new changes, so from other users' point of
view, an undo or redo looks the same as any other kind of change.

To check whether undo is currently available, use the function `Automerge.canUndo(doc)`. It returns
true if the local user has made any changes since the document was created or loaded. You can then
call `Automerge.undo(doc)` to perform an undo. The functions `canRedo()` and `redo()` do the
inverse:

```js
let doc = Automerge.change(Automerge.init(), doc => { doc.birds = [] })
doc = Automerge.change(doc, doc => { doc.birds.push('blackbird') })
doc = Automerge.change(doc, doc => { doc.birds.push('robin') })
// now doc is {birds: ['blackbird', 'robin']}

Automerge.canUndo(doc)    // returns true
doc = Automerge.undo(doc) // now doc is {birds: ['blackbird']}
doc = Automerge.undo(doc) // now doc is {birds: []}
doc = Automerge.redo(doc) // now doc is {birds: ['blackbird']}
doc = Automerge.redo(doc) // now doc is {birds: ['blackbird', 'robin']}
```

You can pass an optional `message` as second argument to `Automerge.undo(doc, message)` and
`Automerge.redo(doc, message)`. This string is used as "commit message" that describes the
undo/redo change, and it appears in the change history (see next section).

### Examining document history

An Automerge document internally saves a complete history of all the changes that were ever made
to it. This enables a nice feature: looking at the document state at past points in time, a.k.a.
*time travel!*

`Automerge.getHistory(doc)` returns a list of all edits made to a document. Each edit is an object
with two properties: `change` is the internal representation of the change (in the same form as
`Automerge.getChanges()` returns), and `snapshot` is the state of the document at the moment just
after that change had been applied.

```js
Automerge.getHistory(doc2)
// [ { change: { message: 'Set x to 1', ... }, snapshot: { x: 1 } },
//   { change: { message: 'Set x to 2', ... }, snapshot: { x: 2 } } ]
```

Within the change object, the property `message` is set to the free-form "commit message" that
was passed in as second argument to `Automerge.change()` (if any). The rest of the change object
is specific to Automerge implementation details, and normally shouldn't need to be interpreted.

If you want to find out what actually changed in a particular edit, rather than inspecting the
change object, it is better to use `Automerge.diff(oldDoc, newDoc)`. This function returns a list
of edits that were made in document `newDoc` since its prior version `oldDoc`. You can pass in
snapshots returned by `Automerge.getHistory()` in order to determine differences between historic
versions.

The data returned by `Automerge.diff()` has the following form:

```js
let history = Automerge.getHistory(doc2)
Automerge.diff(history[2].snapshot, doc2) // get all changes since history[2]
// [ { action: 'set', type: 'map', obj: '...', key: 'x', value: 1 },
//   { action: 'set', type: 'map', obj: '...', key: 'x', value: 2 } ]
```

In the objects returned by `Automerge.diff()`, `obj` indicates the object ID of the object being
edited (matching its `_objectId` property), and `type` indicates whether that object is a `map`,
`list`, or `text`.

The available values for `action` depend on the type of object. For `type: 'map'`, the possible
actions are:

* `action: 'set'`: Then the property `key` is the name of the property being updated. If the value
  assigned to the property is a primitive (string, number, boolean, null), then `value` contains
  that value. If the assigned value is an object (map, list, or text), then `value` contains the
  `_objectId` of that object, and additionally the property `link: true` is set. Moreover, if this
  assignment caused conflicts, then the conflicting values are additionally contained in a
  `conflicts` property.
* `action: 'remove'`: Then the property `key` is the name of the property being removed.

For `type: 'list'` and `type: 'text'`, the possible actions are:

* `action: 'insert'`: Then the property `index` contains the list index at which a new element is
  being inserted, and `value` contains the value inserted there. If the inserted value is an
  object, the `value` property contains its `_objectId`, and the property `link: true` is set.
* `action: 'set'`: Then the property `index` contains the list index to which a new value is being
  assigned, and `value` contains that value. If the assigned value is an object, the `value`
  property contains its `_objectId`, and the property `link: true` is set.
* `action: 'remove'`: Then the property `index` contains the list index that is being removed from
  the list.

## Caveats

The project currently has a number of limitations that you should be aware of:

* No integrity checking: if a buggy (or malicious) device makes corrupted edits, it can cause
  the application state on other devices to become corrupted or go out of sync.
* No security: there is currently no encryption, authentication, or access control.
* Small number of collaborators: Automerge is designed for small-group collaborations. While there
  is no hard limit on the number of devices that can update a document, performance will degrade
  if you go beyond, say, 100 devices or so.
* ...and more, see the [open issues](https://github.com/automerge/automerge/issues).


## Meta

Copyright 2017, Ink & Switch LLC, and University of Cambridge.
Released under the terms of the MIT license (see `LICENSE`).

Created by
[Martin Kleppmann](http://martin.kleppmann.com/),
Orion Henry,
[Peter van Hardenberg](https://twitter.com/pvh),
[Roshan Choxi](https://www.linkedin.com/in/choxi/), and
[Adam Wiggins](http://about.adamwiggins.com/).
