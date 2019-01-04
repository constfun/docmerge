const transit = require('transit-immutable-js')
const uuid = require('./uuid')
const Frontend = require('../frontend')
const Backend = require('../backend')
const { isObject, log } = require('./common')

/**
 * Constructs a new frontend document that reflects the given list of changes.
 */
function docFromChanges(actorId, changes) {
  if (!actorId) throw new RangeError('actorId is required in docFromChanges')
  const doc = Frontend.init({actorId, backend: Backend})
  const [state, _] = Backend.applyChanges(Backend.init(actorId), changes)
  const patch = Backend.getPatch(state)
  patch.state = state
  return Frontend.applyPatch(doc, patch)
}

///// Automerge.* API

function init(actorId) {
  return Frontend.init({actorId, backend: Backend})
}

function load(string, actorId) {
  return docFromChanges(actorId || uuid(), transit.fromJSON(string))
}

function save(doc) {
  const state = Frontend.getBackendState(doc)
  return transit.toJSON(state.getIn(['opSet', 'history']))
}

function merge(localDoc, remoteDoc) {
  if (Frontend.getActorId(localDoc) === Frontend.getActorId(remoteDoc)) {
    throw new RangeError('Cannot merge an actor with itself')
  }
  const localState  = Frontend.getBackendState(localDoc)
  const remoteState = Frontend.getBackendState(remoteDoc)
  const [state, patch] = Backend.merge(localState, remoteState)
  // log('patch', patch)
  if (patch.diffs.length === 0) return localDoc
  patch.state = state
  return Frontend.applyPatch(localDoc, patch)
}

function diff(oldDoc, newDoc) {
  const oldState = Frontend.getBackendState(oldDoc)
  const newState = Frontend.getBackendState(newDoc)
  const changes = Backend.getChanges(oldState, newState)
  const [state, patch] = Backend.applyChanges(oldState, changes)
  return patch.diffs
}

function getChanges(oldDoc, newDoc) {
  const oldState = Frontend.getBackendState(oldDoc)
  const newState = Frontend.getBackendState(newDoc)
  return Backend.getChanges(oldState, newState)
}

function applyChanges(doc, changes) {
  const oldState = Frontend.getBackendState(doc)
  const [newState, patch] = Backend.applyChanges(oldState, changes)
  patch.state = newState
  return Frontend.applyPatch(doc, patch)
}

function getMissingDeps(doc) {
  return Backend.getMissingDeps(Frontend.getBackendState(doc))
}

function equals(val1, val2) {
  if (!isObject(val1) || !isObject(val2)) return val1 === val2
  const keys1 = Object.keys(val1).sort(), keys2 = Object.keys(val2).sort()
  if (keys1.length !== keys2.length) return false
  for (let i = 0; i < keys1.length; i++) {
    if (keys1[i] !== keys2[i]) return false
    if (!equals(val1[keys1[i]], val2[keys2[i]])) return false
  }
  return true
}

function inspect(doc) {
  return JSON.parse(JSON.stringify(doc))
}

function getHistory(doc) {
  const state = Frontend.getBackendState(doc)
  const actor = Frontend.getActorId(doc)

  const history = Backend.getHistory(state)
  return history.map((change, index) => {
    return {
      change,
      get snapshot () {
        return docFromChanges(actor, history.slice(0, index + 1))
      }
    }
  })
}

module.exports = {
  init, load, save, merge, diff, getChanges, applyChanges, getMissingDeps,
  equals, inspect, getHistory, uuid,
  Frontend, Backend,
  DocSet: require('./doc_set'),
  WatchableDoc: require('./watchable_doc'),
  Connection: require('./connection')
}

for (let name of ['change', 'emptyChange', 'canUndo', 'undo', 'canRedo', 'redo',
     'getActorId', 'setActorId', 'getConflicts', 'Text']) {
  module.exports[name] = Frontend[name]
}
