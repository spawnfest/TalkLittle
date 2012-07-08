websocket = null

MOUSE_GRID_ROWS = 4
MOUSE_GRID_COLS = 4

setupEventSource = ->
  source = new EventSource '/eventsource/live'
  
  f = (event) ->
    if event.data == 'sender_you'
      startSending()
    else if event.data == 'sender_someone'
      stopSending()
    else
      addStatus "server sent the following: '#{event.data}'"
  source.addEventListener 'message', f, false
  
  f = (event) ->
    addStatus 'eventsource connected.'
  source.addEventListener 'open', f, false
  
  f = (event) ->
    if event.eventPhase == EventSource.CLOSED
      addStatus 'eventsource was closed.'
  source.addEventListener 'error', f, false
  
startSending = ->
  setupWebsocket()
  addMouseListeners()
  
stopSending = ->
  stopWebsocket()
  removeMouseListeners()
  
setupWebsocket = ->
  if ("MozWebSocket" in window)
    window.WebSocket = MozWebSocket;
  if ("WebSocket" in window)
    # browser supports websockets
    websocket = new WebSocket "ws://localhost:8080/websocket"
    websocket.onopen = ->
      # websocket is connected
      addStatus "websocket connected!"
      # send hello data to server.
      websocket.send "hello server!"
      addStatus "sent message to server: 'hello server'!"
      
    websocket.onmessage = (event) ->
      if event.data == 'sender_you'
        startSending()
      else if event.data == 'sender_someone'
        stopSending()
      else
        addStatus "server sent the following: '#{receivedMsg}'"
    
    websocket.onclose = ->
      # websocket was closed
      websocket = null
      addStatus "websocket was closed"

  else
    # browser does not support websockets
    addStatus "sorry, your browser does not support websockets."
  
stopWebsocket = ->
  websocket?.close()
  
addStatus = (text) ->
  date = new Date
  (document.getElementById 'status').innerHTML =
    (document.getElementById 'status').innerHTML + "#{date}: #{text}<br/>"

addMouseListeners = ->
  (jQuery ':button').click ->
    recordClick this
  
  # TODO use backbone.js in a future version
  mouseGrid = jQuery '<table>'
  for rid in [1..MOUSE_GRID_ROWS]
    row = jQuery '<tr>'
    for cid in [1..MOUSE_GRID_COLS]
      col = jQuery '<td>'
      gridId = MOUSE_GRID_COLS * (rid-1) + cid
      col.addClass "row-#{rid}"
      col.addClass "col-#{cid}"
      row.append col
    mouseGrid.append row
  mouseGrid.attr 'id', 'web-ui-broadcast-mouse-grid'
  mouseGrid.css 'width', '100%'
  mouseGrid.css 'height', '100%'
  mouseGrid.css 'position', 'absolute'
  mouseGrid.css 'top', '0px'
  mouseGrid.css 'left', '0px'
  (jQuery 'body').append mouseGrid

# main
if window.EventSource?
  setupEventSource()
else
  (document.getElementById 'status').innerHTML =
    "Sorry but your browser doesn't support the EventSource API";
