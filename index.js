const STORAGE_KEY = "newtab-bookmarks"

function getJsonValue(key, defaultValue) {
  const value = localStorage.getItem(key) || null
  if (value === null) { return defaultValue }
  return JSON.parse(value)
}

function setJsonValue(key, value) {
  localStorage.setItem(key, JSON.stringify(value))
}

function fetchBookmarks() {
  return getJsonValue(STORAGE_KEY, [])
}

function saveBookmarks(value) {
  return setJsonValue(STORAGE_KEY, value)
}

window.App = function(regionId) {
  const node = document.getElementById(regionId)
  const app = Elm.Main.init({
    node,
    flags: fetchBookmarks(),
  })
  app.ports.updateBookmarks.subscribe(message => {
    saveBookmarks(message)
    app.ports.messageReceiver.send(fetchBookmarks())
  })
}
