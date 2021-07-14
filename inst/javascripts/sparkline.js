function(cell, state) {
 const Plot = createPlotlyComponent(Plotly);
 var x = [<%=js_x%>];
 var y = [<%=js_y%>];
 var x_lower = [<%=js_x_lower%>];
 var x_upper = [<%=js_x_upper%>];
 var x_range = [<%=js_x_range%>];
 var y_range = [<%=js_y_range%>];
 var vline = <%=js_vline%>;
 var text = [<%=js_text%>];
 var height = <%=js_height%>;
 var width = <%=js_width%>;
 var color = [<%=js_color%>];
 var color_errorbar = [<%=js_color_errorbar%>];
 var color_vline = <%=js_color_vline%>;
 
 return React.createElement(Plot, {
    data: [
      <%=data_trace%>
    ],
  "layout": {
    "height": height,
    "width": width,
    "margin": {
      "b": 0,
      "l": 0,
      "t": 0,
      "r": 0,
      "pad": 0
    },
    "xaxis": {
      "domain": [0,1],
      "title": "",
      "range": x_range,
      "zeroline": false,
      "fixedrange": true
    },
    "yaxis": {
      "domain": [0,1],
      "title": "",
      "range": y_range,
      "showgrid": false,
      "zeroline": false,
      "showticklabels": false,
      "fixedrange": true
    },
    "shapes": [
      {
        "type": "line",
        "y0": y_range[0],
        "y1": y_range[1],
        "yref": "paper",
        "x0": vline,
        "x1": vline,
        "line": {
          "color": color_vline
        }
      }
    ],
    "plot_bgcolor": "rgba(0, 0, 0, 0)",
    "paper_bgcolor": "rgba(0, 0, 0, 0)",
    "hoverlabel": {
      "bgcolor": "lightgray"
    },
    "showlegend": false,
    "hovermode": "closest"
  },
  "config": {
    "showSendToCloud": false,
    "displayModeBar": false
  }


  })
}
