{"width": 800, "height": 400, "config": {"title": {"fontSize": 18, "font": "Open Sans", "anchor": "center", "fontColor": "#000000"}, "axisX": {"domain": true, "domainColor": "#000000", "grid": false, "labelFont": "Open Sans", "labelFontSize": 14, "titleFont": "Open Sans", "titleFontSize": 13, "titleFontWeight": "normal", "title": "X Axis Title (units)"}, "axisY": {"domain": true, "grid": false, "gridColor": "#DEDDDD", "gridWidth": 1, "labelFont": "Open Sans", "labelFontSize": 14, "labelAngle": 0, "ticks": false, "titleFont": "Open Sans", "titleFontSize": 14, "titleFontWeight": "normal", "titlePadding": 10, "title": "Y Axis Title (units)"}, "range": {"category": ["#2980B9", "#003366", "#FFAE19", "#FF4500", "#438B28", "#45B39D", "#99ADC1", "#212F3C"]}, "legend": {"labelFont": "Open Sans", "labelFontSize": 14, "symbolType": "square", "symbolSize": 100, "titleFont": "Open Sans", "titleFontSize": 14, "titleFontWeight": "normal", "title": "Legend", "orient": "right", "offset": 0}, "tooltip": {"font": "Open Sans"}, "font": {"font": "Open Sans"}, "view": {"stroke": "transparent", "text": {"font": "Open Sans", "color": ["#2980B9", "#003366", "#FFAE19", "#FF4500", "#438B28", "#45B39D", "#99ADC1", "#212F3C"], "fontSize": 14, "align": "right", "fontWeight": 400, "size": 14}, "bar": {"size": 40, "binSpacing": 1, "continuousBandSize": 30, "discreteBandSize": 30, "fill": ["#2980B9", "#003366", "#FFAE19", "#FF4500", "#438B28", "#45B39D", "#99ADC1", "#212F3C"], "stroke": false}}}, "layer": [{"mark": "bar", "encoding": {"tooltip": {"type": "nominal", "field": "b22"}, "x": {"type": "nominal", "axis": {"title": " "}, "field": "b22", "sort": {"field": "b22", "op": "count", "order": "descending"}}, "y": {"type": "quantitative", "aggregate": "sum", "axis": {"format": "%", "title": ""}, "field": "pct"}}, "height": 400, "selection": {"selector010": {"type": "single", "fields": ["stove"], "bind": {"input": "select", "options": [null, "3-Stone/Open Fire Stove", "Manufactured Stove Traditional", "Koresone Stove", "LPG/Natural Gas Stove", "Electric Stove"], "labels": ["All Responses", "3-Stone/Open Fire Stove", "Manufactured Stove Traditional", "Koresone Stove", "LPG/Natural Gas Stove", "Electric Stove"], "name": "Primary cookstove  "}}, "selector009": {"type": "single", "fields": ["i19a"], "bind": {"input": "select", "options": [null, "Wood purchased", "Wood collected", "Charcoal", "Kerosene", "Crop Residue/Plant Biomass", "LPG/cooking Gas", "Garbage", "Electric", "Piped Natural Gas", "Charcoal Briquettes", "Coal Briquette", "Biomass Briquette", "Coal/lignite", "Other, specify"], "labels": ["All Responses", "Wood Purchased", "Wood Collected", "Charcoal", "Kerosene", "Crop Residue/Plant Biomass", "LPG/Cooking Gas", "Garbage", "Electricity", "Piped Natural Gas", "Charcoal Briquettes", "Coal Briquette", "Biomass Briquette", "Coal/Lignite", "Other"], "name": "Primary source of cooking fuel  "}}, "selector008": {"type": "single", "fields": ["c2"], "bind": {"input": "select", "options": [null, "Yes", "No"], "labels": ["All Responses", "Yes", "No"], "name": "Is the household connected to the national grid?  "}}, "selector006": {"type": "single", "fields": ["locality"], "bind": {"input": "select", "options": [null, 0, 1], "labels": ["National", "Urban", "Rural"], "name": "Locality Breakdown  "}}, "selector007": {"type": "single", "fields": ["elc_aggr_tier"], "bind": {"input": "select", "options": [null, "Tier 0", "Tier 1", "Tier 2", "Tier 3", "Tier 4", "Tier 5"], "labels": ["All Tiers", "Tier 0", "Tier 1", "Tier 2", "Tier 3", "Tier 4", "Tier 5"], "name": "Tiers  "}}}, "transform": [{"joinaggregate": [{"op": "count", "field": "*", "as": "total"}]}, {"calculate": "1 / datum.total", "as": "pct"}], "width": 500}, {"mark": {"type": "text", "align": "center", "baseline": "bottom"}, "encoding": {"text": {"type": "quantitative", "aggregate": "sum", "field": "pct", "format": ".2%"}, "tooltip": {"type": "nominal", "field": "b22"}, "x": {"type": "nominal", "axis": {"title": " "}, "field": "b22", "sort": {"field": "b22", "op": "count", "order": "descending"}}, "y": {"type": "quantitative", "aggregate": "sum", "axis": {"format": "%", "title": ""}, "field": "pct"}}, "height": 400, "transform": [{"joinaggregate": [{"op": "count", "field": "*", "as": "total"}]}, {"calculate": "1 / datum.total", "as": "pct"}], "width": 500}], "data": {"name": "data-ca36fefd3cdcb9d9bab6d8ad0896bcd1"}, "title": {"text": ["Do you use mobile money to make payments over the mobile phone?"], "subtitle": ["You are viewing 2.59% of responses from the Nigeria Multi-Tiered Framework Survey.", " "], "color": "black", "subtitleColor": "black"}, "transform": [{"filter": {"selection": "selector010"}}, {"filter": {"selection": "selector009"}}, {"filter": {"selection": "selector008"}}, {"filter": {"selection": "selector006"}}, {"filter": {"selection": "selector007"}}], "$schema": "https://vega.github.io/schema/vega-lite/v4.8.1.json", "datasets": {"data-ca36fefd3cdcb9d9bab6d8ad0896bcd1": [{"elc_aggr_tier": "Tier 2", "locality": 1, "b22": "Yes", "i19a": "LPG/cooking gas", "c2": "Yes", "stove": "LPG/Natural Gas Stove"}, {"elc_aggr_tier": "Tier 2", "locality": 1, "b22": "Yes", "i19a": "Charcoal", "c2": "Yes", "stove": "NaN"}, {"elc_aggr_tier": "Tier 2", "locality": 1, "b22": "No", "i19a": "Kerosene", "c2": "Yes", "stove": "Koresone Stove"}, {"elc_aggr_tier": "Tier 2", "locality": 1, "b22": "No", "i19a": "Charcoal", "c2": "Yes", "stove": "NaN"}, {"elc_aggr_tier": "Tier 3", "locality": 1, "b22": "Yes", "i19a": "Wood collected", "c2": "Yes", "stove": "3-Stone/Open Fire Stove"}, {"elc_aggr_tier": "Tier 2", "locality": 1, "b22": "Yes", "i19a": "Wood purchased", "c2": "Yes", "stove": "3-Stone/Open Fire Stove"}, {"elc_aggr_tier": "Tier 2", "locality": 1, "b22": "No", "i19a": "Wood purchased", "c2": "Yes", "stove": "3-Stone/Open Fire Stove"}, {"elc_aggr_tier": "Tier 3", "locality": 0, "b22": "Yes", "i19a": "Wood purchased", "c2": "Yes", "stove": "3-Stone/Open Fire Stove"}, {"elc_aggr_tier": "Tier 3", "locality": 1, "b22": "Yes", "i19a": "Wood purchased", "c2": "Yes", "stove": "3-Stone/Open Fire Stove"}, {"elc_aggr_tier": "Tier 3", "locality": 1, "b22": "Yes", "i19a": "Charcoal", "c2": "Yes", "stove": "NaN"}, {"elc_aggr_tier": "Tier 2", "locality": 1, "b22": "Yes", "i19a": "Kerosene", "c2": "Yes", "stove": "NaN"}, {"elc_aggr_tier": "Tier 2", "locality": 1, "b22": "Yes", "i19a": "Kerosene", "c2": "Yes", "stove": "Koresone Stove"}, {"elc_aggr_tier": "Tier 0", "locality": 1, "b22": "Yes", "i19a": "Kerosene", "c2": "Yes", "stove": "Koresone Stove"}, {"elc_aggr_tier": "Tier 3", "locality": 1, "b22": "Yes", "i19a": "Electric", "c2": "Yes", "stove": "NaN"}, {"elc_aggr_tier": "Tier 3", "locality": 1, "b22": "Yes", "i19a": "Kerosene", "c2": "Yes", "stove": "Koresone Stove"}, {"elc_aggr_tier": "Tier 0", "locality": 0, "b22": "No", "i19a": "Wood purchased", "c2": "No", "stove": "3-Stone/Open Fire Stove"}, {"elc_aggr_tier": "Tier 2", "locality": 1, "b22": "Yes", "i19a": "Wood purchased", "c2": "Yes", "stove": "3-Stone/Open Fire Stove"}, {"elc_aggr_tier": "Tier 3", "locality": 0, "b22": "Yes", "i19a": "Wood collected", "c2": "Yes", "stove": "3-Stone/Open Fire Stove"}, {"elc_aggr_tier": "Tier 0", "locality": 0, "b22": "No", "i19a": "Wood collected", "c2": "No", "stove": "3-Stone/Open Fire Stove"}, {"elc_aggr_tier": "Tier 0", "locality": 1, "b22": "Yes", "i19a": "Kerosene", "c2": "Yes", "stove": "Koresone Stove"}, {"elc_aggr_tier": "Tier 0", "locality": 1, "b22": "Yes", "i19a": "Wood purchased", "c2": "Yes", "stove": "NaN"}, {"elc_aggr_tier": "Tier 2", "locality": 1, "b22": "Yes", "i19a": "Wood purchased", "c2": "Yes", "stove": "Manufactured Stove Traditional"}, {"elc_aggr_tier": "Tier 2", "locality": 1, "b22": "Yes", "i19a": "Wood purchased", "c2": "Yes", "stove": "3-Stone/Open Fire Stove"}, {"elc_aggr_tier": "Tier 3", "locality": 1, "b22": "Yes", "i19a": "LPG/cooking gas", "c2": "Yes", "stove": "LPG/Natural Gas Stove"}, {"elc_aggr_tier": "Tier 3", "locality": 1, "b22": "Yes", "i19a": "Kerosene", "c2": "Yes", "stove": "NaN"}, {"elc_aggr_tier": "Tier 3", "locality": 1, "b22": "Yes", "i19a": "Kerosene", "c2": "Yes", "stove": "Koresone Stove"}, {"elc_aggr_tier": "Tier 3", "locality": 1, "b22": "Yes", "i19a": "LPG/cooking gas", "c2": "Yes", "stove": "NaN"}, {"elc_aggr_tier": "Tier 3", "locality": 1, "b22": "Yes", "i19a": "LPG/cooking gas", "c2": "Yes", "stove": "LPG/Natural Gas Stove"}, {"elc_aggr_tier": "Tier 2", "locality": 1, "b22": "Yes", "i19a": "Wood collected", "c2": "Yes", "stove": "3-Stone/Open Fire Stove"}, {"elc_aggr_tier": "Tier 1", "locality": 1, "b22": "No", "i19a": "Wood purchased", "c2": "Yes", "stove": "Manufactured Stove Traditional"}, {"elc_aggr_tier": "Tier 2", "locality": 1, "b22": "No", "i19a": "Wood purchased", "c2": "Yes", "stove": "3-Stone/Open Fire Stove"}, {"elc_aggr_tier": "Tier 1", "locality": 1, "b22": "Yes", "i19a": "Wood purchased", "c2": "Yes", "stove": "3-Stone/Open Fire Stove"}, {"elc_aggr_tier": "Tier 0", "locality": 0, "b22": "Yes", "i19a": "Wood purchased", "c2": "No", "stove": "3-Stone/Open Fire Stove"}, {"elc_aggr_tier": "Tier 3", "locality": 1, "b22": "No", "i19a": "Wood purchased", "c2": "Yes", "stove": "Manufactured Stove Traditional"}, {"elc_aggr_tier": "Tier 0", "locality": 0, "b22": "Yes", "i19a": "Wood purchased", "c2": "No", "stove": "3-Stone/Open Fire Stove"}, {"elc_aggr_tier": "Tier 0", "locality": 0, "b22": "Yes", "i19a": "Wood purchased", "c2": "No", "stove": "3-Stone/Open Fire Stove"}, {"elc_aggr_tier": "Tier 0", "locality": 0, "b22": "Yes", "i19a": "Wood purchased", "c2": "Yes", "stove": "3-Stone/Open Fire Stove"}, {"elc_aggr_tier": "Tier 0", "locality": 0, "b22": "Yes", "i19a": "Kerosene", "c2": "No", "stove": "Koresone Stove"}, {"elc_aggr_tier": "Tier 0", "locality": 0, "b22": "No", "i19a": "Wood purchased", "c2": "No", "stove": "3-Stone/Open Fire Stove"}, {"elc_aggr_tier": "Tier 0", "locality": 0, "b22": "No", "i19a": "Wood purchased", "c2": "No", "stove": "3-Stone/Open Fire Stove"}, {"elc_aggr_tier": "Tier 3", "locality": 0, "b22": "No", "i19a": "Wood purchased", "c2": "Yes", "stove": "3-Stone/Open Fire Stove"}, {"elc_aggr_tier": "Tier 3", "locality": 0, "b22": "Yes", "i19a": "Crop Residue/Plant Biomass", "c2": "Yes", "stove": "3-Stone/Open Fire Stove"}, {"elc_aggr_tier": "Tier 3", "locality": 1, "b22": "No", "i19a": "Charcoal", "c2": "Yes", "stove": "Manufactured Stove Traditional"}, {"elc_aggr_tier": "Tier 4", "locality": 1, "b22": "Yes", "i19a": "Charcoal", "c2": "Yes", "stove": null}, {"elc_aggr_tier": "Tier 4", "locality": 1, "b22": "No", "i19a": "Charcoal", "c2": "Yes", "stove": null}, {"elc_aggr_tier": "Tier 3", "locality": 1, "b22": "Yes", "i19a": "Charcoal", "c2": "Yes", "stove": "Manufactured Stove Traditional"}, {"elc_aggr_tier": "Tier 0", "locality": 1, "b22": "Yes", "i19a": "Kerosene", "c2": "No", "stove": "Koresone Stove"}, {"elc_aggr_tier": "Tier 3", "locality": 1, "b22": "Yes", "i19a": "LPG/cooking gas", "c2": "Yes", "stove": "LPG/Natural Gas Stove"}, {"elc_aggr_tier": "Tier 3", "locality": 1, "b22": "No", "i19a": "Kerosene", "c2": "Yes", "stove": "Koresone Stove"}, {"elc_aggr_tier": "Tier 3", "locality": 1, "b22": "No", "i19a": "Charcoal", "c2": "Yes", "stove": "NaN"}, {"elc_aggr_tier": "Tier 3", "locality": 1, "b22": "Yes", "i19a": "Charcoal", "c2": "Yes", "stove": "Manufactured Stove Traditional"}, {"elc_aggr_tier": "Tier 3", "locality": 1, "b22": "Yes", "i19a": "Charcoal", "c2": "Yes", "stove": "Manufactured Stove Traditional"}, {"elc_aggr_tier": "Tier 2", "locality": 1, "b22": "Yes", "i19a": "LPG/cooking gas", "c2": "Yes", "stove": "LPG/Natural Gas Stove"}, {"elc_aggr_tier": "Tier 1", "locality": 1, "b22": "No", "i19a": "Wood purchased", "c2": "Yes", "stove": "Manufactured Stove Traditional"}, {"elc_aggr_tier": "Tier 3", "locality": 1, "b22": "No", "i19a": "Wood purchased", "c2": "Yes", "stove": "NaN"}, {"elc_aggr_tier": "Tier 3", "locality": 1, "b22": "No", "i19a": "Kerosene", "c2": "Yes", "stove": "NaN"}, {"elc_aggr_tier": "Tier 3", "locality": 1, "b22": "No", "i19a": "Charcoal", "c2": "Yes", "stove": "Manufactured Stove Traditional"}, {"elc_aggr_tier": "Tier 2", "locality": 1, "b22": "No", "i19a": "Charcoal", "c2": "Yes", "stove": "Manufactured Stove Traditional"}, {"elc_aggr_tier": "Tier 2", "locality": 1, "b22": "No", "i19a": "Wood purchased", "c2": "Yes", "stove": "NaN"}, {"elc_aggr_tier": "Tier 3", "locality": 1, "b22": "No", "i19a": "LPG/cooking gas", "c2": "Yes", "stove": "LPG/Natural Gas Stove"}, {"elc_aggr_tier": "Tier 3", "locality": 1, "b22": "No", "i19a": "Kerosene", "c2": "Yes", "stove": "NaN"}, {"elc_aggr_tier": "Tier 3", "locality": 1, "b22": "Yes", "i19a": "LPG/cooking gas", "c2": "Yes", "stove": "LPG/Natural Gas Stove"}, {"elc_aggr_tier": "Tier 3", "locality": 1, "b22": "Yes", "i19a": "Kerosene", "c2": "Yes", "stove": "NaN"}, {"elc_aggr_tier": "Tier 2", "locality": 1, "b22": "Yes", "i19a": "LPG/cooking gas", "c2": "Yes", "stove": "LPG/Natural Gas Stove"}, {"elc_aggr_tier": "Tier 2", "locality": 1, "b22": "Yes", "i19a": "Kerosene", "c2": "Yes", "stove": "NaN"}, {"elc_aggr_tier": "Tier 2", "locality": 1, "b22": "Yes", "i19a": "Kerosene", "c2": "Yes", "stove": "NaN"}, {"elc_aggr_tier": "Tier 2", "locality": 1, "b22": "Yes", "i19a": "Charcoal", "c2": "Yes", "stove": "Manufactured Stove Traditional"}, {"elc_aggr_tier": "Tier 0", "locality": 1, "b22": "Yes", "i19a": "LPG/cooking gas", "c2": "Yes", "stove": "LPG/Natural Gas Stove"}, {"elc_aggr_tier": "Tier 0", "locality": 1, "b22": "Yes", "i19a": "Kerosene", "c2": "Yes", "stove": "NaN"}, {"elc_aggr_tier": "Tier 0", "locality": 1, "b22": "Yes", "i19a": "Kerosene", "c2": "Yes", "stove": "Koresone Stove"}, {"elc_aggr_tier": "Tier 0", "locality": 1, "b22": "Yes", "i19a": "Kerosene", "c2": "Yes", "stove": "NaN"}, {"elc_aggr_tier": "Tier 2", "locality": 1, "b22": "No", "i19a": "LPG/cooking gas", "c2": "Yes", "stove": "LPG/Natural Gas Stove"}, {"elc_aggr_tier": "Tier 2", "locality": 1, "b22": "No", "i19a": "Charcoal", "c2": "Yes", "stove": "NaN"}, {"elc_aggr_tier": "Tier 0", "locality": 0, "b22": "No", "i19a": "Wood collected", "c2": "No", "stove": "3-Stone/Open Fire Stove"}, {"elc_aggr_tier": "Tier 2", "locality": 0, "b22": "Yes", "i19a": "Wood collected", "c2": "No", "stove": "3-Stone/Open Fire Stove"}, {"elc_aggr_tier": "Tier 0", "locality": 0, "b22": "Yes", "i19a": "Wood purchased", "c2": "No", "stove": "3-Stone/Open Fire Stove"}, {"elc_aggr_tier": "Tier 0", "locality": 0, "b22": "Yes", "i19a": "Wood collected", "c2": "No", "stove": "3-Stone/Open Fire Stove"}, {"elc_aggr_tier": "Tier 4", "locality": 0, "b22": "Yes", "i19a": "Wood purchased", "c2": "Yes", "stove": "3-Stone/Open Fire Stove"}, {"elc_aggr_tier": "Tier 0", "locality": 0, "b22": "No", "i19a": "Wood purchased", "c2": "No", "stove": "Manufactured Stove Traditional"}, {"elc_aggr_tier": "Tier 0", "locality": 0, "b22": "No", "i19a": "Wood purchased", "c2": "No", "stove": null}, {"elc_aggr_tier": "Tier 3", "locality": 1, "b22": "Yes", "i19a": "Kerosene", "c2": "Yes", "stove": "Koresone Stove"}, {"elc_aggr_tier": "Tier 3", "locality": 1, "b22": "Yes", "i19a": "Charcoal", "c2": "Yes", "stove": "NaN"}, {"elc_aggr_tier": "Tier 3", "locality": 1, "b22": "Yes", "i19a": "Kerosene", "c2": "Yes", "stove": "Koresone Stove"}, {"elc_aggr_tier": "Tier 2", "locality": 1, "b22": "Yes", "i19a": "Kerosene", "c2": "Yes", "stove": "Koresone Stove"}, {"elc_aggr_tier": "Tier 2", "locality": 1, "b22": "Yes", "i19a": "Charcoal", "c2": "Yes", "stove": "NaN"}, {"elc_aggr_tier": "Tier 3", "locality": 1, "b22": "Yes", "i19a": "LPG/cooking gas", "c2": "Yes", "stove": "LPG/Natural Gas Stove"}, {"elc_aggr_tier": "Tier 3", "locality": 1, "b22": "Yes", "i19a": "Electric", "c2": "Yes", "stove": "NaN"}, {"elc_aggr_tier": "Tier 3", "locality": 1, "b22": "No", "i19a": "LPG/cooking gas", "c2": "Yes", "stove": "LPG/Natural Gas Stove"}, {"elc_aggr_tier": "Tier 3", "locality": 1, "b22": "No", "i19a": "Electric", "c2": "Yes", "stove": "NaN"}, {"elc_aggr_tier": "Tier 4", "locality": 1, "b22": "Yes", "i19a": "Wood purchased", "c2": "Yes", "stove": null}, {"elc_aggr_tier": "Tier 4", "locality": 1, "b22": "Yes", "i19a": "LPG/cooking gas", "c2": "Yes", "stove": "NaN"}, {"elc_aggr_tier": "Tier 3", "locality": 1, "b22": "Yes", "i19a": "Charcoal", "c2": "Yes", "stove": "Manufactured Stove Traditional"}, {"elc_aggr_tier": "Tier 3", "locality": 1, "b22": "Yes", "i19a": "Kerosene", "c2": "Yes", "stove": "NaN"}, {"elc_aggr_tier": "Tier 3", "locality": 1, "b22": "Yes", "i19a": "Electric", "c2": "Yes", "stove": "NaN"}, {"elc_aggr_tier": "Tier 2", "locality": 1, "b22": "No", "i19a": "Charcoal", "c2": "Yes", "stove": "Manufactured Stove Traditional"}, {"elc_aggr_tier": "Tier 4", "locality": 1, "b22": "Yes", "i19a": "Wood purchased", "c2": "Yes", "stove": "3-Stone/Open Fire Stove"}, {"elc_aggr_tier": "Tier 2", "locality": 1, "b22": "No", "i19a": "Wood purchased", "c2": "Yes", "stove": "3-Stone/Open Fire Stove"}, {"elc_aggr_tier": "Tier 4", "locality": 0, "b22": "No", "i19a": "Garbage", "c2": "Yes", "stove": "3-Stone/Open Fire Stove"}, {"elc_aggr_tier": "Tier 4", "locality": 0, "b22": "No", "i19a": "Wood purchased", "c2": "Yes", "stove": "3-Stone/Open Fire Stove"}, {"elc_aggr_tier": "Tier 4", "locality": 1, "b22": "Yes", "i19a": "Garbage", "c2": "Yes", "stove": "3-Stone/Open Fire Stove"}, {"elc_aggr_tier": "Tier 4", "locality": 1, "b22": "No", "i19a": "Garbage", "c2": "Yes", "stove": "3-Stone/Open Fire Stove"}, {"elc_aggr_tier": "Tier 1", "locality": 0, "b22": "No", "i19a": "Wood purchased", "c2": "Yes", "stove": "3-Stone/Open Fire Stove"}, {"elc_aggr_tier": "Tier 3", "locality": 1, "b22": "No", "i19a": "Wood purchased", "c2": "Yes", "stove": null}, {"elc_aggr_tier": "Tier 3", "locality": 1, "b22": "Yes", "i19a": "Wood purchased", "c2": "Yes", "stove": null}, {"elc_aggr_tier": "Tier 3", "locality": 1, "b22": "Yes", "i19a": "LPG/cooking gas", "c2": "Yes", "stove": "LPG/Natural Gas Stove"}, {"elc_aggr_tier": "Tier 2", "locality": 1, "b22": "No", "i19a": "Wood purchased", "c2": "Yes", "stove": "3-Stone/Open Fire Stove"}, {"elc_aggr_tier": "Tier 3", "locality": 0, "b22": "No", "i19a": "Wood purchased", "c2": "Yes", "stove": "3-Stone/Open Fire Stove"}]}}