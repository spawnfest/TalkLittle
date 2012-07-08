websocket = null

setupEventSource = ->
  source = new EventSource '/eventsource/live'
  
  f = (event) ->
    if event.data == 'sender_you'
      setupWebsocket()
    else if event.data == 'sender_someone'
      stopWebsocket()
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
        setupWebsocket()
      else if event.data == 'sender_someone'
        stopWebsocket()
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


# main
if window.EventSource?
  setupEventSource()
else
  (document.getElementById 'status').innerHTML =
    "Sorry but your browser doesn't support the EventSource API";
