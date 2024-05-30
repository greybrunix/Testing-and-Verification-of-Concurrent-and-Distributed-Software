import re
import json
import sys
import os
import subprocess
import webbrowser

def run_harmony(name, web):
    pid = os.fork()
    if pid == 0:
        if web:
            subprocess.run(["harmony", name])
        else:
            subprocess.run(["harmony", name, "--noweb"])
    else:
        os.waitpid(pid, 0)
        name = name[:-3] + "hco"
        generate_html(name)

def generate_html(name):

    with open(name, "r") as f:
        data = f.read()

    pattern_send = r'StoreVar msg_send\",\n\s*\"explain\":.*?\\\"dst\\\":\s(\d+),\s\\\"id\\\":\s(\d+),\s\\\"payload\\\":\s(.*?),\s\\\"src\\\":\s(\d+)'
    pattern_receive = r'StoreVar msg_rcv\",\n\s*\"explain\":.*?\\\"dst\\\":\s(\d+),\s\\\"id\\\":\s(\d+)'
    pattern_drop = r'StoreVar msg_drop\",\n\s*\"explain\":.*?\\\"id\\\":\s(\d+)'
    pattern_dup = r'StoreVar msg_dup\",\n\s*\"explain\":.*?\\\"dst\\\":\s(\d+),\s\\\"id\\\":\s(\d+),\s\\\"payload\\\":\s(.*?),\s\\\"src\\\":\s(\d+)'
    
    pattern = f'{pattern_send}|{pattern_receive}|{pattern_drop}|{pattern_dup}'

    matches = re.findall(pattern, data);

    res = {}
    i = 0
    for match in matches:
        res[f'{i}'] = {}
        # Send
        if match[0] != "":
            res[f'{i}']['type'] = 'send'
            res[f'{i}']['src'] = match[3]
            res[f'{i}']['dst'] = match[0]
            res[f'{i}']['msg'] = match[2]
            res[f'{i}']['id'] = match[1]
        # Receive
        elif match[4] != "":
            res[f'{i}']['type'] = 'receive'
            res[f'{i}']['dst'] = match[4]
            res[f'{i}']['id'] = match[5]
        # Drop
        elif match[6] != "" and lists:
            for j in range(i):
                if (res[f'{j}']['id'] == match[6] and res[f'{j}']['type'] == 'receive'):
                    del res[f'{j}']
                    del res[f'{i}']
        # Dup
        elif match[7] != "":
            for j in range(i):
                if (res[f'{j}']['id'] == match[8] and res[f'{j}']['type'] == 'send'):
                    res[f'{i}']['type'] = 'send'
                    res[f'{i}']['src'] = match[10]
                    res[f'{i}']['dst'] = match[7]
                    res[f'{i}']['msg'] = match[9]
                    res[f'{i}']['id'] = match[8]
        i += 1

    f.close
    
    with open('data.json', 'w+', encoding='utf-8') as f:
        json.dump(res, f, ensure_ascii=False, indent=4)
    with open('data.json', 'r') as file:
        json_data = json.load(file)

    newline = "{newline}"
    html_content = f'''
<!DOCTYPE html>
<html>
<head>
    <meta charset="utf-8">
    <!-- Load d3.js -->
    <script src="https://d3js.org/d3.v4.js"></script>
</head>
<body>
    <!-- Create a div where the graph will take place -->
    <div id="my_dataviz"></div>

    <!-- Include your JavaScript code here -->
    <script>

        var data = '{json.dumps(json_data)}';
        data = JSON.parse(data);

        processData(data);

        function processData(data) {{
            // Initialize variables to store number of columns
            var columnsMax = -Infinity;
            var columnsMin = +Infinity;
            var msgs = Object.keys(data).length;

            // Iterate over each row
            Object.keys(data).forEach(function(key) {{
                var src = parseInt(data[key]['src']);
                var dst = parseInt(data[key]['dst']);

                if (src > columnsMax) {{
                    columnsMax = src;
                }} else if (src < columnsMin) {{
                    columnsMin = src;
                }}

                if (dst > columnsMax) {{
                    columnsMax = dst;
                }} else if (dst < columnsMin) {{
                    columnsMin = dst;
                }}
            }});

            console.log("Min: " + columnsMin + " Max: " + columnsMax);

// set the dimensions and margins of the graph
if (msgs*100 > window.screen.height) {{
    var margin = {{top: 30, right: 30, bottom: 30, left: 30}},
    width = window.screen.width - margin.left - margin.right,
    height = msgs*120 - margin.top - margin.bottom;
}} else {{
    var margin = {{top: 30, right: 30, bottom: 30, left: 30}},
    width = window.screen.width - margin.left - margin.right,
    height = window.screen.height - margin.top - margin.bottom;
}}
    // append the svg object to the body of the page
    svg = d3.select("#my_dataviz")
                .append("svg")
                .attr("width", width + margin.left + margin.right)
                .attr("height", height + margin.top + margin.bottom)
                .append("g")
                .attr("transform",
                     "translate(" + margin.left + "," + margin.top + ")");

  //console.log("Max value: " + collumns);

  // Extract the list of dimensions we want to keep in the plot. Here I keep all except the column called Species
  //dimensions = d3.keys(data[0]).filter(function(d) {{ return d != "Species" }})

  var columnNames = d3.range(columnsMin, columnsMax + 1).map(String);

  var y = {{}};
columnNames.forEach(function(name) {{
    y[name] = d3.scaleLinear()
        .domain([columnsMin, columnsMax])
        .range([height, 0]); // Adjust the range to ensure each row corresponds to one height increment
}});

  // Build the X scale -> it find the best position for each Y axis
  var x = d3.scalePoint()
    .range([0, width])
    .padding(0.1)
    .domain(columnNames);

  // The path function take a row of the json as input, and return x and y coordinates of the line to draw for this raw.
  function path(d) {{
      return d3.line()(columnNames.map(function(p) {{ return [x(p), y[p](d[p])]; }}));
  }}


var box;

// Define arrow marker
svg.append("svg:defs").append("svg:marker")
    .attr("id", "arrow")
    .attr("refX", 12)
    .attr("refY", 6)
    .attr("markerWidth", 30)
    .attr("markerHeight", 30)
    .attr("orient", "auto")
    .append("path")
    .attr("d", "M 0 0 12 6 0 12 3 6")
    .style("fill", "black")
    .on("mouseenter", function(sendRow) {{
        // Show text box on hover
        var sendRow = d3.select(this).datum();
        box = showTextBox(sendRow, this, 1);
    }})
    .on("mouseleave", function(sendRow) {{
        if (box) {{
            box.remove();
            box = null;
        }}
    }})
    .on("click", function(sendRow) {{
        // Show text box on click
        showTextBox(sendRow, this, 0);
    }});

// Define cross marker
svg.append("marker")
    .attr("id", "cross")
    .attr("markerWidth", 50)
    .attr("markerHeight", 50)
    .attr("refX", 6)
    .attr("refY", 4)
    .attr("orient", "auto")
    .append("path")
    .attr("d", "M0,0L8,8M8,0L0,8")
    .attr("stroke", "black")
    .attr("stroke-width", 2)
    .on("mouseenter", function(sendRow) {{
        // Show text box on hover
        var sendRow = d3.select(this).datum();
        box = showTextBox(sendRow, this, 1);
    }})
    .on("mouseleave", function(sendRow) {{
        if (box) {{
            box.remove();
            box = null;
        }}
    }})
    .on("click", function(sendRow) {{
        // Show text box on click
        showTextBox(sendRow, this, 0);
    }});

var seen = [];

function isIdInSeen(id) {{
    return seen.some(function(entry) {{
        return entry[0] === id;
    }});
}}

function getIndexOfId(id) {{
    return seen.findIndex(function(entry) {{
        return entry[0] === id;
    }});
}}

// Draw the lines with arrows
svg.selectAll(".myPathArrows")
    .data(Object.values(data).filter(function(d) {{ return d.type === "send"; }}))
    .enter()
    .append("path")
    .attr("class", "myPathArrows")
    .attr("d", function(sendRow) {{
        var pathData = [];
        var correspondingReceive = Object.values(data).find(function(receiveRow) {{
            return receiveRow.type === "receive" && 
                   receiveRow.dst === sendRow.dst && 
                   receiveRow.id === sendRow.id && 
                   sendRow.src != sendRow.dst;
        }});
        var selfSendReceive = Object.values(data).find(function(receiveRow) {{
            return receiveRow.type === "receive" &&
                   receiveRow.dst === sendRow.dst &&
                   receiveRow.id === sendRow.id &&
                   sendRow.src === sendRow.dst;
        }});

        if (correspondingReceive) {{
            var srcX = x(sendRow.src);
            if (isIdInSeen(sendRow.id)) {{
                var srcY = 10 + getIndexOfId(sendRow.id) * 100; // Adjust the height increment as necessary
            }} else {{
                var srcY = 10 + Object.values(data).indexOf(sendRow) * 100; // Adjust the height increment as necessary
            }}
            var dstY = 10 + Object.values(data).indexOf(correspondingReceive) * 100; // Adjust the height increment as necessary
            var dstX = x(correspondingReceive.dst);
            pathData.push([srcX, srcY], [dstX, dstY]);

            // Calculate angle
            var dx = dstX - srcX;
            var dy = dstY - srcY;
            var angle = Math.atan2(dy, dx) * (180 / Math.PI);

            // Adjust angle based on quadrant
            if (angle > 90 || angle < -90) {{
                angle -= 180;
            }}

            // Add label
            svg.append("text")
                .attr("x", srcX + dx / 5)
                .attr("y", srcY + dy / 5)
                .text(sendRow.msg)
                .attr("text-anchor", "middle")
                .attr("alignment-baseline", "middle")
                .style("fill", "black")
                .style("font-size", "10px") // Adjust font size as needed
                .attr("transform", "rotate(" + angle + "," + (srcX + dx / 5) + "," + (srcY + dy / 5) + ")");
            
            Object.keys(data).forEach(function(key) {{
                if (data[key] === correspondingReceive) {{
                    data[key] = {{}};
                }}
            }});
            seen.push([sendRow.id, Object.values(data).indexOf(sendRow)])
            data[Object.values(data).indexOf(sendRow)] = {{}}

            return d3.line()(pathData);
        }} else if (selfSendReceive) {{
            var srcX = x(sendRow.src);
            if (isIdInSeen(sendRow.id)) {{
                var srcY = 10 + getIndexOfId(sendRow.id) * 100; // Adjust the height increment as necessary
            }} else {{
                var srcY = 10 + Object.values(data).indexOf(sendRow) * 100; // Adjust the height increment as necessary
            }}
            var dstY = 10 + Object.values(data).indexOf(selfSendReceive) * 100; // Adjust the destination Y-coordinate for the curved line

            // Construct path data for the curved line
            pathData.push([srcX, srcY], [srcX + 50, (srcY + dstY) / 2], [srcX, dstY]); // Start, control, and end points of the curve

            // Adjust label positioning and rotation angle
            svg.append("text")
                .attr("x", srcX + 35) // Adjust x position of the label
                .attr("y", srcY + (dstY - srcY) / 2)
                .text(sendRow.msg)
                .attr("text-anchor", "middle")
                .attr("alignment-baseline", "middle")
                .style("fill", "black")
                .style("font-size", "10px")
                .attr("transform", "rotate(" + 90 + "," + (srcX + 35) + "," + (srcY + (dstY - srcY) / 2) + ")");

            Object.keys(data).forEach(function(key) {{
                if (data[key] === correspondingReceive) {{
                    data[key] = {{}};
                }}
            }});
            seen.push([sendRow.id, Object.values(data).indexOf(sendRow)])
            data[Object.values(data).indexOf(sendRow)] = {{}}

            // Return the path data with curve interpolation
            return d3.line().curve(d3.curveBasis)(pathData);
        }}
    }})
    .style("fill", "none")
    .style("stroke", "#69b3a2")
    .style("opacity", 0.5)
    .style("pointer-events", "stroke")
    .style("stroke-width", 2.5)
    .attr("marker-end", "url(#arrow)")
    .on("mouseenter", function(sendRow) {{
        // Show text box on hover
        var sendRow = d3.select(this).datum();
        box = showTextBox(sendRow, this, 1);
    }})
    .on("mouseleave", function(sendRow) {{
        if (box) {{
            box.remove();
            box = null;
        }}
    }})
    .on("click", function(sendRow) {{
        // Show text box on click
        showTextBox(sendRow, this, 0);
    }});

// Draw the lines with crosses
svg.selectAll(".myPathCrosses")
    .data(Object.values(data).filter(function(d) {{ return d.type === "send"; }}))
    .enter()
    .append("path")
    .attr("class", "myPathCrosses")
    .attr("d", function(sendRow) {{
        var pathData = [];
        var noCorrespondingReceive = !Object.values(data).some(function(receiveRow) {{
            return receiveRow.type === "receive" && 
                   receiveRow.dst === sendRow.dst && 
                   receiveRow.id === sendRow.id;
        }});

        if (noCorrespondingReceive && (sendRow.src === sendRow.dst)) {{
            var srcX = x(sendRow.src);
            if (isIdInSeen(sendRow.id)) {{
                var srcY = 10 + getIndexOfId(sendRow.id) * 100; // Adjust the height increment as necessary
            }} else {{
                var srcY = 10 + Object.values(data).indexOf(sendRow) * 100; // Adjust the height increment as necessary
            }}
            var dstX = x(sendRow.src) + 20;
            var dstY = 10 + (Object.values(data).indexOf(sendRow) + 1) * 100; // Adjust the height increment as necessary
            
            // Construct path data for the half curve
            pathData.push([srcX, srcY], [srcX + 50, srcY + (dstY - srcY) / 2], [dstX, dstY]);
            
            // Adjust label positioning and rotation angle
            svg.append("text")
                .attr("x", srcX + 35) // Adjust x position of the label
                .attr("y", srcY + (dstY - srcY) / 2)
                .text(sendRow.msg)
                .attr("text-anchor", "middle")
                .attr("alignment-baseline", "middle")
                .style("fill", "black")
                .style("font-size", "10px")
                .attr("transform", "rotate(" + 80 + "," + (srcX + 35) + "," + (srcY + (dstY - srcY) / 2) + ")");

            Object.keys(data).forEach(function(key) {{
                if (data[key] === noCorrespondingReceive) {{
                    data[key] = {{}};
                }}
            }});
            seen.push([sendRow.id, Object.values(data).indexOf(sendRow)])
            data[Object.values(data).indexOf(sendRow)] = {{}}
            
            return d3.line().curve(d3.curveBasis)(pathData);
        }} else if (noCorrespondingReceive) {{
            var srcX = x(sendRow.src);
            if (isIdInSeen(sendRow.id)) {{
                var srcY = 10 + getIndexOfId(sendRow.id) * 100; // Adjust the height increment as necessary
            }} else {{
                var srcY = 10 + Object.values(data).indexOf(sendRow) * 100; // Adjust the height increment as necessary
            }}
            var dstX = srcX + (x(sendRow.dst) - x(sendRow.src)) * 0.85;
            var dstY = 10 + (Object.values(data).indexOf(sendRow) + 1) * 100; // Adjust the height increment as necessary
            
            pathData.push([srcX, srcY], [dstX, dstY]);

            // Calculate angle
            var dx = dstX - srcX;
            var dy = dstY - srcY;
            var angle = Math.atan2(dy, dx) * (180 / Math.PI);

            // Adjust angle based on quadrant
            if (angle > 90 || angle < -90) {{
                angle -= 180;
            }}

            // Add label
            svg.append("text")
                .attr("x", srcX + (dstX - srcX) / 5)
                .attr("y", srcY + (dstY - srcY) / 5)
                .text(sendRow.msg)
                .attr("text-anchor", "middle")
                .attr("alignment-baseline", "middle")
                .style("fill", "black")
                .style("font-size", "10px") // Adjust font size as needed
                .attr("transform", "rotate(" + angle + "," + (srcX + (dstX - srcX) / 5) + "," + (srcY + (dstY - srcY) / 5) + ")");

            Object.keys(data).forEach(function(key) {{
                if (data[key] === noCorrespondingReceive) {{
                    data[key] = {{}};
                }}
            }});
            seen.push([sendRow.id, Object.values(data).indexOf(sendRow)])
            data[Object.values(data).indexOf(sendRow)] = {{}}

            return d3.line().curve(d3.curveBasis)(pathData);
        }}
    }})
    .style("fill", "none")
    .style("stroke", "#69b3a2")
    .style("opacity", 0.5)
    .style("stroke-width", 2.5)
    .attr("marker-end", "url(#cross)")
    .on("mouseenter", function(sendRow) {{
        // Show text box on hover
        var sendRow = d3.select(this).datum();
        box = showTextBox(sendRow, this, 1);
    }})
    .on("mouseleave", function(sendRow) {{
        if (box) {{
            box.remove();
            box = null;
        }}
    }})
    .on("click", function(sendRow) {{
        // Show text box on click
        showTextBox(sendRow, this, 0);
    }});

function showTextBox(sendRow, element) {{
    var information = "Source: " + sendRow.src + "{newline}";
    information += "Dst: " + sendRow.dst + "{newline}";
    information += "Message: " + sendRow.msg + "{newline}";
    information += "Received: Yes{newline}";

    var mouseCoordinates = d3.mouse(element);
    var mouseX = mouseCoordinates[0];
    var mouseY = mouseCoordinates[1];

    // Show a styled box with the information
    var box = svg.append("g")
        .attr("class", "info-box")
        .attr("transform", "translate(" + (mouseX) + "," + (mouseY + 10) + ")");

    box.append("rect")
        .attr("width", 40 + 10 * sendRow.msg.length) // Adjust the width as needed
        .attr("height", 80) // Adjust the height as needed
        .style("fill", "#fdf6e3") // Adjust the fill color
        .style("stroke", "black"); // Adjust the stroke color

    var lines = information.split("{newline}");

    for (var i=0; i < lines.length; i++) {{
        box.append("text")
            .attr("x", 10) // Adjust the x position of the text
            .attr("y", 20 + i * 15) // Adjust the y position of the text
            .text(lines[i])
            .style("font-size", "8px"); // Adjust the font size
    }}
    box.on("dblclick", function() {{
        d3.select(this).remove();
    }});

    return box;
}}

 // Draw the axis:
  svg.selectAll("myAxis")
    // For each dimension of the dataset I add a 'g' element:
    .data(columnNames).enter()
    .append("g")
    // I translate this element to its right position on the x axis
    .attr("transform", function(d) {{ return "translate(" + x(d) + ")"; }})
    // And I build the axis with the call function
    .each(function(d) {{ 
        d3.select(this).call(d3.axisLeft().scale(y[d]).tickSize(0).tickFormat(function() {{ return null; }}));  }})
    // Add axis title
    .append("text")
      .style("text-anchor", "middle")
      .attr("y", -9)
      .text(function(d) {{ return "Process " + d; }})
      .style("fill", "black")

}};
</script>
    </head>
    <body>
<!-- Create a div where the graph will take place -->
<div id="my_dataviz"></div>
    </body>
</html>
'''

    # Write the HTML content to a file
    if res:
        print('\n' + '-'*40 + '\n')
        print('**JSON for visualization**' + '\n')
        print(res)
        print()

        file_name = f'{name[:-4]}_visualization.html'
        file_path = os.path.abspath(file_name)
        
        with open(file_path, 'w') as file:
            file.write(html_content)

        webbrowser.open(file_path)

        print(f"open file://{file_path} for visualization")

def main():
    name = sys.argv[1] 
    web = ''
    if len(sys.argv) > 2:
        web = sys.argv[2]

    pattern_hny = r'\.hny$'
    pattern_hco = r'\.hco$'
    if re.search(pattern_hny, name):
        if web == '--noweb':
            run_harmony(name, 0)
        else:
            run_harmony(name, 1)
    elif re.search(pattern_hco, name):
        generate_html(name)
if __name__ == '__main__':
    main()
