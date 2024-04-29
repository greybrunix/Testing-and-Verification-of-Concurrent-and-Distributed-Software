// set the dimensions and margins of the graph
var margin = {top: 30, right: 10, bottom: 10, left: 10},
  width = window.screen.width - margin.left - margin.right,
  height = window.screen.height - margin.top - margin.bottom;

// append the svg object to the body of the page
var svg = d3.select("#my_dataviz")
.append("svg")
  .attr("width", width + margin.left + margin.right)
  .attr("height", height + margin.top + margin.bottom)
.append("g")
  .attr("transform",
        "translate(" + margin.left + "," + margin.top + ")");

// Parse the Data
d3.csv("../data.csv", function(data) {
  // Initialize variables to store number os collumns
  var columnsMax = -Infinity;
  var columnsMin = +Infinity;
  var rows = 0;

  // Iterate over each row
  data.forEach(function(row) {
      rows++;

      var src = parseInt(row.src);
      var dst = parseInt(row.dst);

      if (src > columnsMax) {
          columnsMax = src;
      } else if (src < columnsMin) {
          columnsMin = src;
      }

      if (dst > columnsMax) {
          collumns = dst;
      } else if (dst < columnsMin) {
          columnsMin = dst;
      }
  });

  //console.log("Max value: " + collumns);

  // Extract the list of dimensions we want to keep in the plot. Here I keep all except the column called Species
  //dimensions = d3.keys(data[0]).filter(function(d) { return d != "Species" })

  var columnNames = d3.range(columnsMin, columnsMax + 2).map(String);

  var y = {};
columnNames.forEach(function(name) {
    y[name] = d3.scaleLinear()
        .domain([columnsMin, columnsMax])
        .range([height, 0]); // Adjust the range to ensure each row corresponds to one height increment
});

  // Build the X scale -> it find the best position for each Y axis
  var x = d3.scalePoint()
    .range([0, width])
    .padding(1)
    .domain(columnNames);

  // The path function take a row of the csv as input, and return x and y coordinates of the line to draw for this raw.
  function path(d) {
      return d3.line()(columnNames.map(function(p) { return [x(p), y[p](d[p])]; }));
  }

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
    .style("fill", "black");

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
    .attr("stroke-width", 2);

// Draw the lines with arrows
svg.selectAll(".myPathArrows")
    .data(data.filter(function(d) { return d.type === "send"; }))
    .enter()
    .append("path")
    .attr("class", "myPathArrows")
    .attr("d", function(sendRow) {
        var pathData = [];
        var correspondingReceive = data.find(function(receiveRow) {
            return receiveRow.type === "receive" && 
                   receiveRow.dst === sendRow.dst && 
                   receiveRow.id === sendRow.id && 
                   sendRow.src != sendRow.dst;
        });
        var selfSendReceive = data.find(function(receiveRow) {
            return receiveRow.type === "receive" &&
                   receiveRow.dst === sendRow.dst &&
                   receiveRow.id === sendRow.id &&
                   sendRow.src === sendRow.dst;
        });

        if (correspondingReceive) {
            var srcX = x(sendRow.src);
            var srcY = data.indexOf(sendRow) * 100; // Adjust the height increment as necessary
            var dstX = x(correspondingReceive.dst);
            var dstY = data.indexOf(correspondingReceive) * 100; // Adjust the height increment as necessary
            pathData.push([srcX, srcY], [dstX, dstY]);

            // Calculate angle
            var dx = dstX - srcX;
            var dy = dstY - srcY;
            var angle = Math.atan2(dy, dx) * (180 / Math.PI);

            // Adjust angle based on quadrant
            if (angle > 90 || angle < -90) {
                angle -= 180;
            }

            // Add label
            svg.append("text")
                .attr("x", (srcX + dstX) / 2)
                .attr("y", (srcY + dstY) / 2)
                .text(sendRow.msg)
                .attr("text-anchor", "middle")
                .attr("alignment-baseline", "middle")
                .style("fill", "black")
                .style("font-size", "10px") // Adjust font size as needed
                .attr("transform", "rotate(" + angle + "," + ((srcX + dstX) / 2) + "," + ((srcY + dstY) / 2) + ")");
            
            return d3.line()(pathData);
        } else if (selfSendReceive) {
            var srcX = x(sendRow.src);
            var srcY = data.indexOf(sendRow) * 100; // Adjust the height increment as necessary
            var dstY = data.indexOf(selfSendReceive) * 100; // Adjust the destination Y-coordinate for the curved line

            // Construct path data for the curved line
            pathData.push([srcX, srcY], [srcX + 50, (srcY + dstY) / 2], [srcX, dstY]); // Start, control, and end points of the curve

            // Adjust label positioning and rotation angle
            svg.append("text")
                .attr("x", srcX + 35) // Adjust x position of the label
                .attr("y", (srcY + dstY) / 2)
                .text(sendRow.msg)
                .attr("text-anchor", "middle")
                .attr("alignment-baseline", "middle")
                .style("fill", "black")
                .style("font-size", "10px")
                .attr("transform", "rotate(" + 90 + "," + (srcX + 35) + "," + ((srcY + dstY) / 2) + ")");

            // Return the path data with curve interpolation
            return d3.line().curve(d3.curveBasis)(pathData);
        }
    })
    .style("fill", "none")
    .style("stroke", "#69b3a2")
    .style("opacity", 0.5)
    .style("pointer-events", "stroke")
    .style("stroke-width", 2.5)
    .attr("marker-end", "url(#arrow)")
    .on("click", function(sendRow){
        var information = "Source: " + sendRow.src + "\n";
        information += "Dst: " + sendRow.dst + "\n";
        information += "Message: " + sendRow.msg + "\n";
        information += "Received: Yes\n";
        information += "ID: " + sendRow.id + "\n";
        
        var mouseCoordinates = d3.mouse(this);
        var mouseX = mouseCoordinates[0];
        var mouseY = mouseCoordinates[1];

         // Show a styled box with the information
        var box = svg.append("g")
            .attr("class", "info-box")
            .attr("transform", "translate(" + (mouseX) + "," + (mouseY + 10) + ")");

        box.append("rect")
            .attr("width", 100) // Adjust the width as needed
            .attr("height", 100) // Adjust the height as needed
            .style("fill", "#fdf6e3") // Adjust the fill color
            .style("stroke", "black"); // Adjust the stroke color

        var lines = information.split("\n");

        for (var i=0; i < lines.length; i++) {
            box.append("text")
                .attr("x", 10) // Adjust the x position of the text
                .attr("y", 20 + i * 15) // Adjust the y position of the text
                .text(lines[i])
                .style("font-size", "8px"); // Adjust the font size
        }

        box.on("dblclick", function() {
            d3.select(this).remove();
        });
    });

// Draw the lines with crosses
svg.selectAll(".myPathCrosses")
    .data(data.filter(function(d) { return d.type === "send"; }))
    .enter()
    .append("path")
    .attr("class", "myPathCrosses")
    .attr("d", function(sendRow) {
        var pathData = [];
        var noCorrespondingReceive = !data.some(function(receiveRow) {
            return receiveRow.type === "receive" && 
                   receiveRow.dst === sendRow.dst && 
                   receiveRow.id === sendRow.id;
        });

        if (noCorrespondingReceive && (sendRow.src === sendRow.dst)) {
            var srcX = x(sendRow.src);
            var srcY = data.indexOf(sendRow) * 100; // Adjust the height increment as necessary
            var dstX = x(sendRow.src) + 20;
            var dstY = (data.indexOf(sendRow) + 1) * 100; // Adjust the height increment as necessary
            
            // Construct path data for the half curve
            pathData.push([srcX, srcY], [srcX + 50, srcY + (dstY - srcY) / 2], [dstX, dstY]);
            
            // Adjust label positioning and rotation angle
            svg.append("text")
                .attr("x", srcX + 35) // Adjust x position of the label
                .attr("y", (srcY + dstY) / 2)
                .text(sendRow.msg)
                .attr("text-anchor", "middle")
                .attr("alignment-baseline", "middle")
                .style("fill", "black")
                .style("font-size", "10px")
                .attr("transform", "rotate(" + 80 + "," + (srcX + 35) + "," + ((srcY + dstY) / 2) + ")");

            
            return d3.line().curve(d3.curveBasis)(pathData);
        } else if (noCorrespondingReceive) {
            var srcX = x(sendRow.src);
            var srcY = data.indexOf(sendRow) * 100; // Adjust the height increment as necessary
            var dstX = x(sendRow.dst) - 100;
            var dstY = (data.indexOf(sendRow) + 1) * 100; // Adjust the height increment as necessary
            
            pathData.push([srcX, srcY], [dstX, dstY]);

            // Calculate angle
            var dx = dstX - srcX;
            var dy = dstY - srcY;
            var angle = Math.atan2(dy, dx) * (180 / Math.PI);

            // Adjust angle based on quadrant
            if (angle > 90 || angle < -90) {
                angle -= 180;
            }

            // Add label
            svg.append("text")
                .attr("x", (srcX + dstX) / 2)
                .attr("y", (srcY + dstY) / 2)
                .text(sendRow.msg)
                .attr("text-anchor", "middle")
                .attr("alignment-baseline", "middle")
                .style("fill", "black")
                .style("font-size", "10px") // Adjust font size as needed
                .attr("transform", "rotate(" + angle + "," + ((srcX + dstX) / 2) + "," + ((srcY + dstY) / 2) + ")");

            return d3.line().curve(d3.curveBasis)(pathData);
        }
    })
    .style("fill", "none")
    .style("stroke", "#69b3a2")
    .style("opacity", 0.5)
    .style("stroke-width", 2.5)
    .attr("marker-end", "url(#cross)")
    .on("click", function(sendRow){
        var information = "Source: " + sendRow.src + "\n";
        information += "Dst: " + sendRow.dst + "\n";
        information += "Message: " + sendRow.msg + "\n";
        information += "Received: No\n";
        information += "ID: " + sendRow.id + "\n";

        var mouseCoordinates = d3.mouse(this);
        var mouseX = mouseCoordinates[0];
        var mouseY = mouseCoordinates[1];

         // Show a styled box with the information
        var box = svg.append("g")
            .attr("class", "info-box")
            .attr("transform", "translate(" + (mouseX) + "," + (mouseY + 10) + ")");

        box.append("rect")
            .attr("width", 100) // Adjust the width as needed
            .attr("height", 100) // Adjust the height as needed
            .style("fill", "#fdf6e3") // Adjust the fill color
            .style("stroke", "black"); // Adjust the stroke color

        var lines = information.split("\n");

        for (var i=0; i < lines.length; i++) {
            box.append("text")
                .attr("x", 10) // Adjust the x position of the text
                .attr("y", 20 + i * 15) // Adjust the y position of the text
                .text(lines[i])
                .style("font-size", "8px"); // Adjust the font size
        }

        box.on("dblclick", function() {
            d3.select(this).remove();
        });
    });

// Draw the axis:
  svg.selectAll("myAxis")
    // For each dimension of the dataset I add a 'g' element:
    .data(columnNames).enter()
    .append("g")
    // I translate this element to its right position on the x axis
    .attr("transform", function(d) { return "translate(" + x(d) + ")"; })
    // And I build the axis with the call function
    .each(function(d) { 
        d3.select(this).call(d3.axisLeft().scale(y[d]).tickSize(0).tickFormat(function() { return null; }));  })
    // Add axis title
    .append("text")
      .style("text-anchor", "middle")
      .attr("y", -9)
      .text(function(d) { return "Process " + d; })
      .style("fill", "black")

});
