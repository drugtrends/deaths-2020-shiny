var dimension=0;$(document).on("shiny:connected",(function(n){dimension=window.innerWidth,Shiny.setInputValue("dimension",dimension)})),$(window).resize((function(n){dimension=window.innerWidth,Shiny.setInputValue("dimension",dimension)}));