Set WshShell = CreateObject("WScript.Shell" ) 
WshShell.Run chr(34) & "C:\ShinyApps\DCRShinyApp\DCR_WQ_App.bat" & Chr(34), 0 
Set WshShell = Nothing 