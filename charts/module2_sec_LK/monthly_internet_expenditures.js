{"width": 800, "height": 400, "config": {"title": {"fontSize": 18, "font": "Times New Roman", "anchor": "center", "fontColor": "#000000"}, "axisX": {"domain": true, "domainColor": "#000000", "grid": false, "labelFont": "Times New Roman", "labelFontSize": 14, "titleFont": "Times New Roman", "titleFontSize": 14, "titleFontWeight": "normal", "title": "X Axis Title (units)"}, "axisY": {"domain": true, "grid": false, "gridColor": "#DEDDDD", "gridWidth": 1, "labelFont": "Times New Roman", "labelFontSize": 14, "labelAngle": 0, "ticks": false, "titleFont": "Times New Roman", "titleFontSize": 14, "titleFontWeight": "normal", "titlePadding": 10, "title": "Y Axis Title (units)"}, "range": {"category": ["#2980B9", "#003366", "#FFAE19", "#D68910", "#E67E22", "#D35400", "#FF4500", "#FF6162", "#438B28", "#45B39D", "#99ADC1", "#212F3C"]}, "legend": {"labelFont": "Times New Roman", "labelFontSize": 14, "symbolType": "square", "symbolSize": 100, "titleFont": "Times New Roman", "titleFontSize": 14, "titleFontWeight": "normal", "title": "Legend", "orient": "right", "offset": 0}, "tooltip": {"font": "Open Sans"}, "view": {"stroke": "transparent", "text": {"font": "Times New Roman", "color": ["#2980B9", "#003366", "#FFAE19", "#D68910", "#E67E22", "#D35400", "#FF4500", "#FF6162", "#438B28", "#45B39D", "#99ADC1", "#212F3C"], "fontSize": 14, "align": "right", "fontWeight": 400, "size": 14}, "bar": {"size": 40, "binSpacing": 1, "continuousBandSize": 30, "discreteBandSize": 30, "fill": ["#2980B9", "#003366", "#FFAE19", "#D68910", "#E67E22", "#D35400", "#FF4500", "#FF6162", "#438B28", "#45B39D", "#99ADC1", "#212F3C"], "stroke": false}}}, "concat": [{"mark": "bar", "encoding": {"tooltip": [{"type": "nominal", "field": "l_expenditure", "title": "Response (Binned)"}, {"type": "quantitative", "aggregate": "sum", "field": "pct", "format": ".2%", "title": "Percentage"}], "x": {"type": "quantitative", "axis": {"title": "Nigerian Naira"}, "bin": {"extent": [150, 15000], "step": 1000}, "field": "l_expenditure", "scale": {"clamp": true, "domain": [150, 15000]}}, "y": {"type": "quantitative", "aggregate": "sum", "axis": {"format": "%"}, "field": "pct", "title": ""}}, "height": 400, "selection": {"selector034": {"type": "single", "fields": ["stove"], "bind": {"input": "select", "options": [null, "3-Stone/Open Fire Stove", "Manufactured Stove Traditional", "Koresone Stove", "LPG/Natural Gas Stove", "Electric Stove"], "labels": ["All Stove Types", "3-Stone/Open Fire Stove", "Manufactured Stove Traditional", "Koresone Stove", "LPG/Natural Gas Stove", "Electric Stove"], "name": "Primary cookstove type  "}}, "selector033": {"type": "single", "fields": ["i19a"], "bind": {"input": "select", "options": [null, "Wood purchased", "Wood collected", "Charcoal", "Kerosene", "Crop Residue/Plant Biomass", "LPG/cooking gas", "Garbage", "Electric", "Piped Natural Gas", "Charcoal Briquettes", "Coal Briquette", "Biomass Briquette", "Coal/lignite", "Other, specify"], "labels": ["All Fuel Types", "Wood purchased", "Wood collected", "Charcoal", "Kerosene", "Crop Residue/Plant Biomass", "LPG/cooking gas", "Garbage", "Electric", "Piped Natural Gas", "Charcoal Briquettes", "Coal Briquette", "Biomass Briquette", "Coal/lignite", "Other"], "name": "Primary source of cooking fuel  "}}, "selector032": {"type": "single", "fields": ["c182"], "bind": {"input": "select", "options": [null, "Dry-cell battery", "National Grid Connection", "No electricity", "Electric generator", "Other, specify", "Solar Multi-Light Product", "Solar Home System", "Solar Lantern", "Local Mini Grid connection", "Rechargeable Battery"], "labels": ["All Responses", "Dry-cell battery", "National Grid Connection", "No electricity", "Electric generator", "Other", "Solar Multi-Light Product", "Solar Home System", "Solar Lantern", "Local Mini Grid Connection", "Rechargeable Battery"], "name": "Primary source of electricity  "}}, "selector025": {"type": "single", "fields": ["locality"], "bind": {"input": "select", "options": [null, 0, 1], "labels": ["National", "Urban", "Rural"], "name": "Locality Breakdown  "}}}, "title": {"text": ["Internet and Household Communication Expenditures per Month"], "color": "black", "subtitleColor": "black"}, "transform": [{"joinaggregate": [{"op": "count", "field": "*", "as": "total"}]}, {"calculate": "1 / datum.total", "as": "pct"}, {"bin": {"maxbins": 20}, "field": "m", "as": "mbin"}, {"filter": {"selection": "selector034"}}, {"filter": {"selection": "selector033"}}, {"filter": {"selection": "selector032"}}, {"filter": {"selection": "selector025"}}], "width": 500}], "data": {"name": "data-1e041346fe2a71820b07622442490677"}, "title": {"text": ["", "", "This chart includes responses from 3.76% of households in the Nigeria MTF survey sample (total size = 3,668).", "Use the dropdown filters to interact with the data."], "anchor": "start", "baseline": "bottom", "font": "Times New Roman", "fontSize": 15, "fontStyle": "italic", "fontWeight": "normal", "orient": "bottom"}, "$schema": "https://vega.github.io/schema/vega-lite/v4.8.1.json", "datasets": {"data-1e041346fe2a71820b07622442490677": [{"hh_id": 1012500802501, "elc_aggr_tier": "Tier 2", "l_expenditure": 1200.0, "locality": 1, "c182": "Dry-cell battery", "i19a": "Charcoal", "stove": "Manufactured Stove Traditional"}, {"hh_id": 1012509342901, "elc_aggr_tier": "Tier 0", "l_expenditure": 3000.0, "locality": 1, "c182": "National Grid Connection", "i19a": "Wood purchased", "stove": "NaN"}, {"hh_id": 1012509342901, "elc_aggr_tier": "Tier 0", "l_expenditure": 3000.0, "locality": 1, "c182": "National Grid Connection", "i19a": "Charcoal", "stove": null}, {"hh_id": 1012509343201, "elc_aggr_tier": "Tier 2", "l_expenditure": 3500.0, "locality": 1, "c182": "National Grid Connection", "i19a": "LPG/cooking gas", "stove": "LPG/Natural Gas Stove"}, {"hh_id": 1012509343201, "elc_aggr_tier": "Tier 2", "l_expenditure": 3500.0, "locality": 1, "c182": "National Grid Connection", "i19a": "Charcoal", "stove": "NaN"}, {"hh_id": 1013501240301, "elc_aggr_tier": "Tier 2", "l_expenditure": 1200.0, "locality": 1, "c182": "Dry-cell battery", "i19a": "LPG/cooking gas", "stove": "NaN"}, {"hh_id": 1013501240301, "elc_aggr_tier": "Tier 2", "l_expenditure": 1200.0, "locality": 1, "c182": "Dry-cell battery", "i19a": "Charcoal", "stove": "Manufactured Stove Traditional"}, {"hh_id": 1013501240601, "elc_aggr_tier": "Tier 2", "l_expenditure": 1000.0, "locality": 1, "c182": "Dry-cell battery", "i19a": "Kerosene", "stove": "Koresone Stove"}, {"hh_id": 1013501240601, "elc_aggr_tier": "Tier 2", "l_expenditure": 1000.0, "locality": 1, "c182": "National Grid Connection", "i19a": "Charcoal", "stove": "NaN"}, {"hh_id": 1013503461401, "elc_aggr_tier": "Tier 3", "l_expenditure": 1000.0, "locality": 1, "c182": "Dry-cell battery", "i19a": "Wood purchased", "stove": "3-Stone/Open Fire Stove"}, {"hh_id": 1013507621001, "elc_aggr_tier": "Tier 3", "l_expenditure": 888.0, "locality": 1, "c182": "National Grid Connection", "i19a": "Kerosene", "stove": "NaN"}, {"hh_id": 1013507621001, "elc_aggr_tier": "Tier 3", "l_expenditure": 888.0, "locality": 1, "c182": "National Grid Connection", "i19a": "Charcoal", "stove": null}, {"hh_id": 1021512081301, "elc_aggr_tier": "Tier 4", "l_expenditure": 5200.0, "locality": 1, "c182": "Dry-cell battery", "i19a": "Charcoal", "stove": "Manufactured Stove Traditional"}, {"hh_id": 2003500842501, "elc_aggr_tier": "Tier 3", "l_expenditure": 2500.0, "locality": 1, "c182": null, "i19a": "Wood purchased", "stove": "3-Stone/Open Fire Stove"}, {"hh_id": 2009502560301, "elc_aggr_tier": "Tier 2", "l_expenditure": 3000.0, "locality": 1, "c182": null, "i19a": "Wood purchased", "stove": null}, {"hh_id": 2011512806101, "elc_aggr_tier": "Tier 3", "l_expenditure": 1500.0, "locality": 1, "c182": null, "i19a": "Kerosene", "stove": "Koresone Stove"}, {"hh_id": 2011512806101, "elc_aggr_tier": "Tier 3", "l_expenditure": 1500.0, "locality": 1, "c182": null, "i19a": "Charcoal", "stove": "NaN"}, {"hh_id": 2011512806101, "elc_aggr_tier": "Tier 3", "l_expenditure": 1500.0, "locality": 1, "c182": null, "i19a": "Electric", "stove": "NaN"}, {"hh_id": 2011514582103, "elc_aggr_tier": "Tier 2", "l_expenditure": 10000.0, "locality": 1, "c182": null, "i19a": "Kerosene", "stove": "NaN"}, {"hh_id": 2011514582103, "elc_aggr_tier": "Tier 2", "l_expenditure": 10000.0, "locality": 1, "c182": null, "i19a": "Wood purchased", "stove": "3-Stone/Open Fire Stove"}, {"hh_id": 2011519100101, "elc_aggr_tier": "Tier 2", "l_expenditure": 1500.0, "locality": 1, "c182": null, "i19a": "Kerosene", "stove": "NaN"}, {"hh_id": 2011519100912, "elc_aggr_tier": "Tier 0", "l_expenditure": 1000.0, "locality": 1, "c182": null, "i19a": "Kerosene", "stove": "Koresone Stove"}, {"hh_id": 2011519101102, "elc_aggr_tier": "Tier 3", "l_expenditure": 888.0, "locality": 1, "c182": null, "i19a": "Electric", "stove": "NaN"}, {"hh_id": 2011519101102, "elc_aggr_tier": "Tier 3", "l_expenditure": 888.0, "locality": 1, "c182": null, "i19a": "Kerosene", "stove": "Koresone Stove"}, {"hh_id": 3008505644301, "elc_aggr_tier": "Tier 2", "l_expenditure": 2600.0, "locality": 1, "c182": null, "i19a": "Wood purchased", "stove": "Manufactured Stove Traditional"}, {"hh_id": 3008513021901, "elc_aggr_tier": "Tier 3", "l_expenditure": 2000.0, "locality": 1, "c182": null, "i19a": "Wood purchased", "stove": "3-Stone/Open Fire Stove"}, {"hh_id": 3008515461501, "elc_aggr_tier": "Tier 1", "l_expenditure": 3000.0, "locality": 1, "c182": null, "i19a": "LPG/cooking gas", "stove": "LPG/Natural Gas Stove"}, {"hh_id": 3008515461501, "elc_aggr_tier": "Tier 1", "l_expenditure": 3000.0, "locality": 1, "c182": null, "i19a": "Kerosene", "stove": "NaN"}, {"hh_id": 3008515461701, "elc_aggr_tier": "Tier 2", "l_expenditure": 2000.0, "locality": 1, "c182": null, "i19a": "Wood purchased", "stove": "Manufactured Stove Traditional"}, {"hh_id": 3008515461701, "elc_aggr_tier": "Tier 2", "l_expenditure": 2000.0, "locality": 1, "c182": null, "i19a": "Kerosene", "stove": "NaN"}, {"hh_id": 3008515462201, "elc_aggr_tier": "Tier 3", "l_expenditure": 3000.0, "locality": 1, "c182": null, "i19a": "Kerosene", "stove": "Koresone Stove"}, {"hh_id": 3008515462201, "elc_aggr_tier": "Tier 3", "l_expenditure": 3000.0, "locality": 1, "c182": null, "i19a": "LPG/cooking gas", "stove": "NaN"}, {"hh_id": 4018502701901, "elc_aggr_tier": "Tier 0", "l_expenditure": 2000.0, "locality": 0, "c182": null, "i19a": "Wood purchased", "stove": null}, {"hh_id": 4018502702301, "elc_aggr_tier": "Tier 0", "l_expenditure": 2000.0, "locality": 0, "c182": null, "i19a": "Wood purchased", "stove": "3-Stone/Open Fire Stove"}, {"hh_id": 4018502703301, "elc_aggr_tier": "Tier 0", "l_expenditure": 3500.0, "locality": 0, "c182": null, "i19a": "Wood purchased", "stove": "3-Stone/Open Fire Stove"}, {"hh_id": 4018502704602, "elc_aggr_tier": "Tier 0", "l_expenditure": 500.0, "locality": 0, "c182": null, "i19a": "Wood collected", "stove": "3-Stone/Open Fire Stove"}, {"hh_id": 4024520642301, "elc_aggr_tier": "Tier 2", "l_expenditure": 2500.0, "locality": 1, "c182": null, "i19a": "LPG/cooking gas", "stove": "LPG/Natural Gas Stove"}, {"hh_id": 4024520642301, "elc_aggr_tier": "Tier 2", "l_expenditure": 2500.0, "locality": 1, "c182": null, "i19a": "Electric", "stove": "NaN"}, {"hh_id": 10010506861001, "elc_aggr_tier": "Tier 3", "l_expenditure": 3000.0, "locality": 1, "c182": null, "i19a": "Crop Residue/Plant Biomass", "stove": "3-Stone/Open Fire Stove"}, {"hh_id": 10011506083301, "elc_aggr_tier": "Tier 1", "l_expenditure": 3000.0, "locality": 1, "c182": null, "i19a": "Kerosene", "stove": "Koresone Stove"}, {"hh_id": 10011514800402, "elc_aggr_tier": "Tier 0", "l_expenditure": 1000.0, "locality": 1, "c182": null, "i19a": "Kerosene", "stove": "Koresone Stove"}, {"hh_id": 10011514801601, "elc_aggr_tier": "Tier 3", "l_expenditure": 3500.0, "locality": 1, "c182": null, "i19a": "Charcoal", "stove": "Manufactured Stove Traditional"}, {"hh_id": 10011514801601, "elc_aggr_tier": "Tier 3", "l_expenditure": 3500.0, "locality": 1, "c182": null, "i19a": "Kerosene", "stove": "NaN"}, {"hh_id": 10011514804702, "elc_aggr_tier": "Tier 2", "l_expenditure": 2500.0, "locality": 1, "c182": null, "i19a": "LPG/cooking gas", "stove": "NaN"}, {"hh_id": 10011514804702, "elc_aggr_tier": "Tier 2", "l_expenditure": 2500.0, "locality": 1, "c182": null, "i19a": "Kerosene", "stove": "Koresone Stove"}, {"hh_id": 10012510700101, "elc_aggr_tier": "Tier 3", "l_expenditure": 6000.0, "locality": 1, "c182": null, "i19a": "LPG/cooking gas", "stove": "LPG/Natural Gas Stove"}, {"hh_id": 10012523072501, "elc_aggr_tier": "Tier 3", "l_expenditure": 2000.0, "locality": 1, "c182": null, "i19a": "Kerosene", "stove": "NaN"}, {"hh_id": 10012523072501, "elc_aggr_tier": "Tier 3", "l_expenditure": 2000.0, "locality": 1, "c182": null, "i19a": "Charcoal", "stove": null}, {"hh_id": 10013500988302, "elc_aggr_tier": "Tier 0", "l_expenditure": 2000.0, "locality": 1, "c182": null, "i19a": "Charcoal", "stove": "Manufactured Stove Traditional"}, {"hh_id": 10013500988302, "elc_aggr_tier": "Tier 0", "l_expenditure": 2000.0, "locality": 1, "c182": null, "i19a": "Kerosene", "stove": "NaN"}, {"hh_id": 10013500992102, "elc_aggr_tier": "Tier 3", "l_expenditure": 200.0, "locality": 1, "c182": null, "i19a": "Wood purchased", "stove": "3-Stone/Open Fire Stove"}, {"hh_id": 10013500993101, "elc_aggr_tier": "Tier 2", "l_expenditure": 1000.0, "locality": 1, "c182": null, "i19a": "Kerosene", "stove": "NaN"}, {"hh_id": 10013500993101, "elc_aggr_tier": "Tier 2", "l_expenditure": 1000.0, "locality": 1, "c182": null, "i19a": "Charcoal", "stove": "Manufactured Stove Traditional"}, {"hh_id": 10013500993101, "elc_aggr_tier": "Tier 2", "l_expenditure": 1000.0, "locality": 1, "c182": null, "i19a": "Wood purchased", "stove": "NaN"}, {"hh_id": 10013516800401, "elc_aggr_tier": "Tier 3", "l_expenditure": 1000.0, "locality": 1, "c182": null, "i19a": "Charcoal", "stove": "Manufactured Stove Traditional"}, {"hh_id": 10013516802201, "elc_aggr_tier": "Tier 2", "l_expenditure": 2500.0, "locality": 1, "c182": null, "i19a": "Charcoal", "stove": null}, {"hh_id": 10013516802201, "elc_aggr_tier": "Tier 2", "l_expenditure": 2500.0, "locality": 1, "c182": null, "i19a": "Kerosene", "stove": "NaN"}, {"hh_id": 10014511561901, "elc_aggr_tier": "Tier 1", "l_expenditure": 1500.0, "locality": 1, "c182": null, "i19a": "Charcoal", "stove": "Manufactured Stove Traditional"}, {"hh_id": 10014511566601, "elc_aggr_tier": "Tier 3", "l_expenditure": 1000.0, "locality": 1, "c182": null, "i19a": "LPG/cooking gas", "stove": "LPG/Natural Gas Stove"}, {"hh_id": 10014511566601, "elc_aggr_tier": "Tier 3", "l_expenditure": 1000.0, "locality": 1, "c182": null, "i19a": "Kerosene", "stove": "NaN"}, {"hh_id": 10014516940303, "elc_aggr_tier": "Tier 0", "l_expenditure": 1500.0, "locality": 1, "c182": null, "i19a": "Kerosene", "stove": "Koresone Stove"}, {"hh_id": 10014516940303, "elc_aggr_tier": "Tier 0", "l_expenditure": 1500.0, "locality": 1, "c182": null, "i19a": "LPG/cooking gas", "stove": "NaN"}, {"hh_id": 10014516940303, "elc_aggr_tier": "Tier 0", "l_expenditure": 1500.0, "locality": 1, "c182": null, "i19a": "Charcoal", "stove": "NaN"}, {"hh_id": 10014516940501, "elc_aggr_tier": "Tier 0", "l_expenditure": 1500.0, "locality": 1, "c182": null, "i19a": "LPG/cooking gas", "stove": "LPG/Natural Gas Stove"}, {"hh_id": 10014516941102, "elc_aggr_tier": "Tier 3", "l_expenditure": 2000.0, "locality": 1, "c182": null, "i19a": "LPG/cooking gas", "stove": "LPG/Natural Gas Stove"}, {"hh_id": 10014516941102, "elc_aggr_tier": "Tier 3", "l_expenditure": 2000.0, "locality": 1, "c182": null, "i19a": "Kerosene", "stove": "NaN"}, {"hh_id": 10014516941304, "elc_aggr_tier": "Tier 1", "l_expenditure": 2400.0, "locality": 1, "c182": null, "i19a": "LPG/cooking gas", "stove": "LPG/Natural Gas Stove"}, {"hh_id": 10014516941501, "elc_aggr_tier": "Tier 1", "l_expenditure": 2500.0, "locality": 1, "c182": null, "i19a": "Kerosene", "stove": "Koresone Stove"}, {"hh_id": 10014516941503, "elc_aggr_tier": "Tier 1", "l_expenditure": 1400.0, "locality": 1, "c182": null, "i19a": "Charcoal", "stove": "Manufactured Stove Traditional"}, {"hh_id": 10014516942505, "elc_aggr_tier": "Tier 3", "l_expenditure": 2000.0, "locality": 1, "c182": null, "i19a": "LPG/cooking gas", "stove": "LPG/Natural Gas Stove"}, {"hh_id": 10014516942604, "elc_aggr_tier": "Tier 3", "l_expenditure": 1000.0, "locality": 1, "c182": null, "i19a": "LPG/cooking gas", "stove": "LPG/Natural Gas Stove"}, {"hh_id": 10014516942604, "elc_aggr_tier": "Tier 3", "l_expenditure": 1000.0, "locality": 1, "c182": null, "i19a": "Kerosene", "stove": "NaN"}, {"hh_id": 10015507540201, "elc_aggr_tier": "Tier 0", "l_expenditure": 1200.0, "locality": 1, "c182": null, "i19a": "LPG/cooking gas", "stove": "LPG/Natural Gas Stove"}, {"hh_id": 10015508141401, "elc_aggr_tier": "Tier 3", "l_expenditure": 2000.0, "locality": 1, "c182": null, "i19a": "Kerosene", "stove": "Koresone Stove"}, {"hh_id": 10015508141901, "elc_aggr_tier": "Tier 2", "l_expenditure": 1500.0, "locality": 1, "c182": null, "i19a": "Wood purchased", "stove": "3-Stone/Open Fire Stove"}, {"hh_id": 10015508143001, "elc_aggr_tier": "Tier 4", "l_expenditure": 2000.0, "locality": 1, "c182": null, "i19a": "LPG/cooking gas", "stove": "LPG/Natural Gas Stove"}, {"hh_id": 10015534241001, "elc_aggr_tier": "Tier 3", "l_expenditure": 1000.0, "locality": 1, "c182": null, "i19a": "LPG/cooking gas", "stove": "LPG/Natural Gas Stove"}, {"hh_id": 10015534860803, "elc_aggr_tier": "Tier 3", "l_expenditure": 4000.0, "locality": 1, "c182": null, "i19a": "Charcoal", "stove": null}, {"hh_id": 10015534860901, "elc_aggr_tier": "Tier 3", "l_expenditure": 6000.0, "locality": 1, "c182": null, "i19a": "Charcoal", "stove": "NaN"}, {"hh_id": 10015534860901, "elc_aggr_tier": "Tier 3", "l_expenditure": 6000.0, "locality": 1, "c182": null, "i19a": "Wood purchased", "stove": "Manufactured Stove Traditional"}, {"hh_id": 10015534862101, "elc_aggr_tier": "Tier 2", "l_expenditure": 5000.0, "locality": 1, "c182": null, "i19a": "LPG/cooking gas", "stove": "LPG/Natural Gas Stove"}, {"hh_id": 10015534862101, "elc_aggr_tier": "Tier 2", "l_expenditure": 5000.0, "locality": 1, "c182": null, "i19a": "Kerosene", "stove": "NaN"}, {"hh_id": 10015534863201, "elc_aggr_tier": "Tier 3", "l_expenditure": 3000.0, "locality": 1, "c182": null, "i19a": "Wood purchased", "stove": "Manufactured Stove Traditional"}, {"hh_id": 10015534863201, "elc_aggr_tier": "Tier 3", "l_expenditure": 3000.0, "locality": 1, "c182": null, "i19a": "Charcoal", "stove": "NaN"}, {"hh_id": 10015536807101, "elc_aggr_tier": "Tier 3", "l_expenditure": 1000.0, "locality": 1, "c182": null, "i19a": "Wood purchased", "stove": "3-Stone/Open Fire Stove"}, {"hh_id": 10015536807101, "elc_aggr_tier": "Tier 3", "l_expenditure": 1000.0, "locality": 1, "c182": null, "i19a": "Kerosene", "stove": "NaN"}, {"hh_id": 10015543840401, "elc_aggr_tier": "Tier 3", "l_expenditure": 1200.0, "locality": 1, "c182": null, "i19a": "Charcoal", "stove": "Manufactured Stove Traditional"}, {"hh_id": 10015543840401, "elc_aggr_tier": "Tier 3", "l_expenditure": 1200.0, "locality": 1, "c182": null, "i19a": "Wood purchased", "stove": "NaN"}, {"hh_id": 10015543841801, "elc_aggr_tier": "Tier 1", "l_expenditure": 3000.0, "locality": 1, "c182": null, "i19a": "Piped Natural Gas", "stove": "LPG/Natural Gas Stove"}, {"hh_id": 10015543841801, "elc_aggr_tier": "Tier 1", "l_expenditure": 3000.0, "locality": 1, "c182": null, "i19a": "Kerosene", "stove": "NaN"}, {"hh_id": 10015543841801, "elc_aggr_tier": "Tier 1", "l_expenditure": 3000.0, "locality": 1, "c182": null, "i19a": "Electric", "stove": "NaN"}, {"hh_id": 10015544720203, "elc_aggr_tier": "Tier 3", "l_expenditure": 3000.0, "locality": 1, "c182": null, "i19a": "Kerosene", "stove": "Koresone Stove"}, {"hh_id": 10015544721801, "elc_aggr_tier": "Tier 3", "l_expenditure": 3000.0, "locality": 1, "c182": null, "i19a": "Wood purchased", "stove": "3-Stone/Open Fire Stove"}, {"hh_id": 10015544721801, "elc_aggr_tier": "Tier 3", "l_expenditure": 3000.0, "locality": 1, "c182": null, "i19a": "Charcoal", "stove": "NaN"}, {"hh_id": 10015544722302, "elc_aggr_tier": "Tier 2", "l_expenditure": 5000.0, "locality": 1, "c182": null, "i19a": "Wood purchased", "stove": "3-Stone/Open Fire Stove"}, {"hh_id": 10015544722302, "elc_aggr_tier": "Tier 2", "l_expenditure": 5000.0, "locality": 1, "c182": null, "i19a": "Charcoal", "stove": "NaN"}, {"hh_id": 10015544723401, "elc_aggr_tier": "Tier 3", "l_expenditure": 1000.0, "locality": 1, "c182": null, "i19a": "Charcoal", "stove": "Manufactured Stove Traditional"}, {"hh_id": 10015544723401, "elc_aggr_tier": "Tier 3", "l_expenditure": 1000.0, "locality": 1, "c182": null, "i19a": "Kerosene", "stove": "NaN"}, {"hh_id": 10016503820103, "elc_aggr_tier": "Tier 0", "l_expenditure": 3000.0, "locality": 1, "c182": null, "i19a": "Kerosene", "stove": "Koresone Stove"}, {"hh_id": 10016503821301, "elc_aggr_tier": "Tier 0", "l_expenditure": 3000.0, "locality": 1, "c182": null, "i19a": "LPG/cooking gas", "stove": "LPG/Natural Gas Stove"}, {"hh_id": 10016503821301, "elc_aggr_tier": "Tier 0", "l_expenditure": 3000.0, "locality": 1, "c182": null, "i19a": "Kerosene", "stove": "NaN"}, {"hh_id": 10016503822105, "elc_aggr_tier": "Tier 0", "l_expenditure": 1500.0, "locality": 1, "c182": null, "i19a": "LPG/cooking gas", "stove": "LPG/Natural Gas Stove"}, {"hh_id": 10016506647501, "elc_aggr_tier": "Tier 1", "l_expenditure": 2500.0, "locality": 1, "c182": null, "i19a": "LPG/cooking gas", "stove": "LPG/Natural Gas Stove"}, {"hh_id": 10016506650201, "elc_aggr_tier": "Tier 3", "l_expenditure": 2000.0, "locality": 1, "c182": null, "i19a": "Charcoal", "stove": "Manufactured Stove Traditional"}, {"hh_id": 10016506650201, "elc_aggr_tier": "Tier 3", "l_expenditure": 2000.0, "locality": 1, "c182": null, "i19a": "Kerosene", "stove": "NaN"}, {"hh_id": 10016520000101, "elc_aggr_tier": "Tier 1", "l_expenditure": 1500.0, "locality": 1, "c182": null, "i19a": "Charcoal", "stove": "Manufactured Stove Traditional"}, {"hh_id": 10016520002301, "elc_aggr_tier": "Tier 2", "l_expenditure": 7000.0, "locality": 1, "c182": null, "i19a": "Charcoal", "stove": "Manufactured Stove Traditional"}, {"hh_id": 10016520002301, "elc_aggr_tier": "Tier 2", "l_expenditure": 7000.0, "locality": 1, "c182": null, "i19a": "Charcoal", "stove": "NaN"}, {"hh_id": 10016520003501, "elc_aggr_tier": "Tier 1", "l_expenditure": 3000.0, "locality": 1, "c182": null, "i19a": "LPG/cooking gas", "stove": "LPG/Natural Gas Stove"}, {"hh_id": 10016520003501, "elc_aggr_tier": "Tier 1", "l_expenditure": 3000.0, "locality": 1, "c182": null, "i19a": "Kerosene", "stove": "NaN"}, {"hh_id": 10016520003702, "elc_aggr_tier": "Tier 2", "l_expenditure": 400.0, "locality": 1, "c182": null, "i19a": "Charcoal", "stove": "Manufactured Stove Traditional"}, {"hh_id": 10017518560501, "elc_aggr_tier": "Tier 2", "l_expenditure": 3000.0, "locality": 1, "c182": null, "i19a": "LPG/cooking gas", "stove": "LPG/Natural Gas Stove"}, {"hh_id": 10017518560501, "elc_aggr_tier": "Tier 2", "l_expenditure": 3000.0, "locality": 1, "c182": null, "i19a": "Charcoal", "stove": "NaN"}, {"hh_id": 10017518560601, "elc_aggr_tier": "Tier 3", "l_expenditure": 5000.0, "locality": 1, "c182": null, "i19a": "LPG/cooking gas", "stove": "LPG/Natural Gas Stove"}, {"hh_id": 10017518560801, "elc_aggr_tier": "Tier 3", "l_expenditure": 5000.0, "locality": 1, "c182": null, "i19a": "LPG/cooking gas", "stove": "LPG/Natural Gas Stove"}, {"hh_id": 10017518560902, "elc_aggr_tier": "Tier 3", "l_expenditure": 1500.0, "locality": 1, "c182": null, "i19a": "Charcoal", "stove": "Manufactured Stove Traditional"}, {"hh_id": 10017518560903, "elc_aggr_tier": "Tier 1", "l_expenditure": 2000.0, "locality": 1, "c182": null, "i19a": "LPG/cooking gas", "stove": "LPG/Natural Gas Stove"}, {"hh_id": 10017518560903, "elc_aggr_tier": "Tier 1", "l_expenditure": 2000.0, "locality": 1, "c182": null, "i19a": "Kerosene", "stove": "NaN"}, {"hh_id": 10017518561001, "elc_aggr_tier": "Tier 2", "l_expenditure": 2500.0, "locality": 1, "c182": null, "i19a": "LPG/cooking gas", "stove": "LPG/Natural Gas Stove"}, {"hh_id": 10017518561301, "elc_aggr_tier": "Tier 2", "l_expenditure": 3000.0, "locality": 1, "c182": null, "i19a": "LPG/cooking gas", "stove": "LPG/Natural Gas Stove"}, {"hh_id": 10017518561301, "elc_aggr_tier": "Tier 2", "l_expenditure": 3000.0, "locality": 1, "c182": null, "i19a": "Charcoal", "stove": "NaN"}, {"hh_id": 10017518561301, "elc_aggr_tier": "Tier 2", "l_expenditure": 3000.0, "locality": 1, "c182": null, "i19a": "Wood purchased", "stove": "NaN"}, {"hh_id": 10017518561402, "elc_aggr_tier": "Tier 4", "l_expenditure": 1900.0, "locality": 1, "c182": null, "i19a": "LPG/cooking gas", "stove": "LPG/Natural Gas Stove"}, {"hh_id": 10017518561403, "elc_aggr_tier": "Tier 2", "l_expenditure": 2500.0, "locality": 1, "c182": null, "i19a": "LPG/cooking gas", "stove": "LPG/Natural Gas Stove"}, {"hh_id": 10017518561404, "elc_aggr_tier": "Tier 2", "l_expenditure": 4500.0, "locality": 1, "c182": null, "i19a": "Kerosene", "stove": "Koresone Stove"}, {"hh_id": 10032510622101, "elc_aggr_tier": "Tier 3", "l_expenditure": 6000.0, "locality": 1, "c182": null, "i19a": "Charcoal", "stove": null}, {"hh_id": 10034509800401, "elc_aggr_tier": "Tier 0", "l_expenditure": 2000.0, "locality": 0, "c182": null, "i19a": "Charcoal", "stove": "Manufactured Stove Traditional"}, {"hh_id": 10034509800401, "elc_aggr_tier": "Tier 0", "l_expenditure": 2000.0, "locality": 0, "c182": null, "i19a": "Kerosene", "stove": "NaN"}, {"hh_id": 10034509801101, "elc_aggr_tier": "Tier 4", "l_expenditure": 1500.0, "locality": 0, "c182": null, "i19a": "LPG/cooking gas", "stove": "LPG/Natural Gas Stove"}, {"hh_id": 10034509803501, "elc_aggr_tier": "Tier 4", "l_expenditure": 1400.0, "locality": 0, "c182": null, "i19a": "Charcoal", "stove": "Manufactured Stove Traditional"}, {"hh_id": 10036504121902, "elc_aggr_tier": "Tier 2", "l_expenditure": 12000.0, "locality": 1, "c182": null, "i19a": "Wood purchased", "stove": "3-Stone/Open Fire Stove"}, {"hh_id": 10036504122101, "elc_aggr_tier": "Tier 1", "l_expenditure": 2500.0, "locality": 1, "c182": null, "i19a": "Wood collected", "stove": "3-Stone/Open Fire Stove"}, {"hh_id": 10041501340401, "elc_aggr_tier": "Tier 3", "l_expenditure": 1000.0, "locality": 1, "c182": null, "i19a": "Wood purchased", "stove": "Manufactured Stove Traditional"}, {"hh_id": 10041501340401, "elc_aggr_tier": "Tier 3", "l_expenditure": 1000.0, "locality": 1, "c182": null, "i19a": "Kerosene", "stove": "NaN"}, {"hh_id": 10041501342101, "elc_aggr_tier": "Tier 2", "l_expenditure": 2000.0, "locality": 1, "c182": null, "i19a": "Wood purchased", "stove": "3-Stone/Open Fire Stove"}, {"hh_id": 11001500205901, "elc_aggr_tier": "Tier 3", "l_expenditure": 1000.0, "locality": 1, "c182": null, "i19a": "Kerosene", "stove": "Koresone Stove"}, {"hh_id": 11001500205901, "elc_aggr_tier": "Tier 3", "l_expenditure": 1000.0, "locality": 1, "c182": null, "i19a": "Charcoal", "stove": "NaN"}, {"hh_id": 11001506941801, "elc_aggr_tier": "Tier 3", "l_expenditure": 1000.0, "locality": 0, "c182": null, "i19a": "Wood purchased", "stove": "3-Stone/Open Fire Stove"}, {"hh_id": 11002502205201, "elc_aggr_tier": "Tier 3", "l_expenditure": 1800.0, "locality": 1, "c182": null, "i19a": "Wood purchased", "stove": "3-Stone/Open Fire Stove"}, {"hh_id": 11002502205201, "elc_aggr_tier": "Tier 3", "l_expenditure": 1800.0, "locality": 1, "c182": null, "i19a": "Kerosene", "stove": "NaN"}, {"hh_id": 11002502206303, "elc_aggr_tier": "Tier 1", "l_expenditure": 5000.0, "locality": 1, "c182": null, "i19a": "Wood purchased", "stove": "3-Stone/Open Fire Stove"}, {"hh_id": 11002502206303, "elc_aggr_tier": "Tier 1", "l_expenditure": 5000.0, "locality": 1, "c182": null, "i19a": "Charcoal", "stove": "NaN"}, {"hh_id": 11002502206303, "elc_aggr_tier": "Tier 1", "l_expenditure": 5000.0, "locality": 1, "c182": null, "i19a": "Kerosene", "stove": "NaN"}, {"hh_id": 11002502206303, "elc_aggr_tier": "Tier 1", "l_expenditure": 5000.0, "locality": 1, "c182": null, "i19a": "Kerosene", "stove": "NaN"}, {"hh_id": 11002502206701, "elc_aggr_tier": "Tier 3", "l_expenditure": 1500.0, "locality": 1, "c182": null, "i19a": "Kerosene", "stove": "Koresone Stove"}, {"hh_id": 11002502206801, "elc_aggr_tier": "Tier 1", "l_expenditure": 1200.0, "locality": 1, "c182": null, "i19a": "LPG/cooking gas", "stove": "LPG/Natural Gas Stove"}, {"hh_id": 11002502206802, "elc_aggr_tier": "Tier 2", "l_expenditure": 2500.0, "locality": 1, "c182": null, "i19a": "Wood purchased", "stove": "3-Stone/Open Fire Stove"}, {"hh_id": 11002502206802, "elc_aggr_tier": "Tier 2", "l_expenditure": 2500.0, "locality": 1, "c182": null, "i19a": "Kerosene", "stove": "NaN"}, {"hh_id": 11007505263001, "elc_aggr_tier": "Tier 2", "l_expenditure": 150.0, "locality": 0, "c182": null, "i19a": "Wood purchased", "stove": "3-Stone/Open Fire Stove"}, {"hh_id": 11007505263001, "elc_aggr_tier": "Tier 2", "l_expenditure": 150.0, "locality": 0, "c182": null, "i19a": "Charcoal", "stove": "NaN"}, {"hh_id": 11009517487701, "elc_aggr_tier": "Tier 3", "l_expenditure": 1000.0, "locality": 1, "c182": null, "i19a": "Wood purchased", "stove": "3-Stone/Open Fire Stove"}, {"hh_id": 11009517488002, "elc_aggr_tier": "Tier 2", "l_expenditure": 6000.0, "locality": 1, "c182": null, "i19a": "LPG/cooking gas", "stove": "LPG/Natural Gas Stove"}, {"hh_id": 11009517488002, "elc_aggr_tier": "Tier 2", "l_expenditure": 6000.0, "locality": 1, "c182": null, "i19a": "Kerosene", "stove": "NaN"}, {"hh_id": 11009517488601, "elc_aggr_tier": "Tier 2", "l_expenditure": 2000.0, "locality": 1, "c182": null, "i19a": "Kerosene", "stove": "Koresone Stove"}, {"hh_id": 11009517488601, "elc_aggr_tier": "Tier 2", "l_expenditure": 2000.0, "locality": 1, "c182": null, "i19a": "Electric", "stove": "NaN"}, {"hh_id": 11010513304201, "elc_aggr_tier": "Tier 4", "l_expenditure": 15000.0, "locality": 1, "c182": null, "i19a": "LPG/cooking gas", "stove": "NaN"}, {"hh_id": 11010513304201, "elc_aggr_tier": "Tier 4", "l_expenditure": 15000.0, "locality": 1, "c182": null, "i19a": "Wood purchased", "stove": "3-Stone/Open Fire Stove"}, {"hh_id": 11010513304401, "elc_aggr_tier": "Tier 4", "l_expenditure": 12000.0, "locality": 1, "c182": null, "i19a": "LPG/cooking gas", "stove": "LPG/Natural Gas Stove"}, {"hh_id": 11010513304701, "elc_aggr_tier": "Tier 3", "l_expenditure": 3800.0, "locality": 1, "c182": null, "i19a": "Wood purchased", "stove": "3-Stone/Open Fire Stove"}, {"hh_id": 11010513306301, "elc_aggr_tier": "Tier 4", "l_expenditure": 1000.0, "locality": 1, "c182": null, "i19a": "Wood purchased", "stove": null}, {"hh_id": 11010513306301, "elc_aggr_tier": "Tier 4", "l_expenditure": 1000.0, "locality": 1, "c182": null, "i19a": "LPG/cooking gas", "stove": "NaN"}, {"hh_id": 11010513307201, "elc_aggr_tier": "Tier 4", "l_expenditure": 2000.0, "locality": 1, "c182": null, "i19a": "LPG/cooking gas", "stove": "LPG/Natural Gas Stove"}, {"hh_id": 11010513361403, "elc_aggr_tier": "Tier 3", "l_expenditure": 2000.0, "locality": 1, "c182": null, "i19a": "LPG/cooking gas", "stove": "LPG/Natural Gas Stove"}, {"hh_id": 11010513361403, "elc_aggr_tier": "Tier 3", "l_expenditure": 2000.0, "locality": 1, "c182": null, "i19a": "Kerosene", "stove": "NaN"}, {"hh_id": 11010516484002, "elc_aggr_tier": "Tier 1", "l_expenditure": 3000.0, "locality": 1, "c182": null, "i19a": "Wood purchased", "stove": "3-Stone/Open Fire Stove"}, {"hh_id": 11010516484101, "elc_aggr_tier": "Tier 3", "l_expenditure": 1800.0, "locality": 1, "c182": null, "i19a": "LPG/cooking gas", "stove": "LPG/Natural Gas Stove"}, {"hh_id": 11011514942201, "elc_aggr_tier": "Tier 4", "l_expenditure": 1000.0, "locality": 1, "c182": null, "i19a": "LPG/cooking gas", "stove": "LPG/Natural Gas Stove"}, {"hh_id": 11011514942201, "elc_aggr_tier": "Tier 4", "l_expenditure": 1000.0, "locality": 1, "c182": null, "i19a": "Kerosene", "stove": "NaN"}, {"hh_id": 11011514942201, "elc_aggr_tier": "Tier 4", "l_expenditure": 1000.0, "locality": 1, "c182": null, "i19a": "Charcoal", "stove": "NaN"}, {"hh_id": 11011514943101, "elc_aggr_tier": "Tier 3", "l_expenditure": 500.0, "locality": 1, "c182": null, "i19a": "Kerosene", "stove": "Koresone Stove"}, {"hh_id": 11011514944401, "elc_aggr_tier": "Tier 3", "l_expenditure": 1800.0, "locality": 1, "c182": null, "i19a": "LPG/cooking gas", "stove": "LPG/Natural Gas Stove"}, {"hh_id": 11011514944401, "elc_aggr_tier": "Tier 3", "l_expenditure": 1800.0, "locality": 1, "c182": null, "i19a": "Electric", "stove": "NaN"}, {"hh_id": 11011514944801, "elc_aggr_tier": "Tier 2", "l_expenditure": 3000.0, "locality": 1, "c182": null, "i19a": "Kerosene", "stove": "Koresone Stove"}, {"hh_id": 11011514945303, "elc_aggr_tier": "Tier 3", "l_expenditure": 1000.0, "locality": 1, "c182": null, "i19a": "Kerosene", "stove": "Koresone Stove"}, {"hh_id": 11011514947702, "elc_aggr_tier": "Tier 3", "l_expenditure": 1000.0, "locality": 1, "c182": null, "i19a": "Charcoal", "stove": "Manufactured Stove Traditional"}, {"hh_id": 11011514947702, "elc_aggr_tier": "Tier 3", "l_expenditure": 1000.0, "locality": 1, "c182": null, "i19a": "Kerosene", "stove": "NaN"}, {"hh_id": 11011514947702, "elc_aggr_tier": "Tier 3", "l_expenditure": 1000.0, "locality": 1, "c182": null, "i19a": "Electric", "stove": "NaN"}, {"hh_id": 11011514948002, "elc_aggr_tier": "Tier 3", "l_expenditure": 2000.0, "locality": 1, "c182": null, "i19a": "Kerosene", "stove": "Koresone Stove"}, {"hh_id": 11011518104902, "elc_aggr_tier": "Tier 3", "l_expenditure": 3000.0, "locality": 1, "c182": null, "i19a": "LPG/cooking gas", "stove": "LPG/Natural Gas Stove"}, {"hh_id": 11011518107401, "elc_aggr_tier": "Tier 0", "l_expenditure": 1000.0, "locality": 1, "c182": null, "i19a": "Wood purchased", "stove": "3-Stone/Open Fire Stove"}, {"hh_id": 11011518107401, "elc_aggr_tier": "Tier 0", "l_expenditure": 1000.0, "locality": 1, "c182": null, "i19a": "Charcoal", "stove": "NaN"}, {"hh_id": 11011518107401, "elc_aggr_tier": "Tier 0", "l_expenditure": 1000.0, "locality": 1, "c182": null, "i19a": "Kerosene", "stove": "NaN"}, {"hh_id": 11011518108503, "elc_aggr_tier": "Tier 2", "l_expenditure": 10000.0, "locality": 1, "c182": null, "i19a": "LPG/cooking gas", "stove": "LPG/Natural Gas Stove"}, {"hh_id": 11011520780101, "elc_aggr_tier": "Tier 2", "l_expenditure": 3000.0, "locality": 1, "c182": null, "i19a": "Kerosene", "stove": "Koresone Stove"}, {"hh_id": 11011520780301, "elc_aggr_tier": "Tier 3", "l_expenditure": 1000.0, "locality": 1, "c182": null, "i19a": "Wood purchased", "stove": "3-Stone/Open Fire Stove"}, {"hh_id": 11011520780301, "elc_aggr_tier": "Tier 3", "l_expenditure": 1000.0, "locality": 1, "c182": null, "i19a": "Kerosene", "stove": "NaN"}, {"hh_id": 11011520780301, "elc_aggr_tier": "Tier 3", "l_expenditure": 1000.0, "locality": 1, "c182": null, "i19a": "Electric", "stove": "NaN"}, {"hh_id": 11011520780802, "elc_aggr_tier": "Tier 3", "l_expenditure": 2500.0, "locality": 1, "c182": null, "i19a": "Kerosene", "stove": "Koresone Stove"}, {"hh_id": 11011520780802, "elc_aggr_tier": "Tier 3", "l_expenditure": 2500.0, "locality": 1, "c182": null, "i19a": "Electric", "stove": "NaN"}, {"hh_id": 11011520780903, "elc_aggr_tier": "Tier 4", "l_expenditure": 1800.0, "locality": 1, "c182": null, "i19a": "LPG/cooking gas", "stove": "LPG/Natural Gas Stove"}, {"hh_id": 11011520780903, "elc_aggr_tier": "Tier 4", "l_expenditure": 1800.0, "locality": 1, "c182": null, "i19a": "Kerosene", "stove": "NaN"}, {"hh_id": 11011520780903, "elc_aggr_tier": "Tier 4", "l_expenditure": 1800.0, "locality": 1, "c182": null, "i19a": "Electric", "stove": "NaN"}, {"hh_id": 11011520781003, "elc_aggr_tier": "Tier 3", "l_expenditure": 1300.0, "locality": 1, "c182": null, "i19a": "Electric", "stove": "Electric Stove"}, {"hh_id": 11011520781003, "elc_aggr_tier": "Tier 3", "l_expenditure": 1300.0, "locality": 1, "c182": null, "i19a": "Kerosene", "stove": "NaN"}, {"hh_id": 11011520781101, "elc_aggr_tier": "Tier 3", "l_expenditure": 1200.0, "locality": 1, "c182": null, "i19a": "Kerosene", "stove": "NaN"}, {"hh_id": 11011520781101, "elc_aggr_tier": "Tier 3", "l_expenditure": 1200.0, "locality": 1, "c182": null, "i19a": "Electric", "stove": "Electric Stove"}, {"hh_id": 11012518400211, "elc_aggr_tier": "Tier 3", "l_expenditure": 3000.0, "locality": 1, "c182": null, "i19a": "Piped Natural Gas", "stove": "LPG/Natural Gas Stove"}, {"hh_id": 11012526380204, "elc_aggr_tier": "Tier 3", "l_expenditure": 3000.0, "locality": 1, "c182": null, "i19a": "Kerosene", "stove": "Koresone Stove"}, {"hh_id": 11012526381007, "elc_aggr_tier": "Tier 2", "l_expenditure": 1000.0, "locality": 1, "c182": null, "i19a": "Kerosene", "stove": "Koresone Stove"}, {"hh_id": 11012526383402, "elc_aggr_tier": "Tier 1", "l_expenditure": 2000.0, "locality": 1, "c182": null, "i19a": "Wood purchased", "stove": "3-Stone/Open Fire Stove"}, {"hh_id": 11012526384101, "elc_aggr_tier": "Tier 2", "l_expenditure": 3600.0, "locality": 1, "c182": null, "i19a": "Kerosene", "stove": "Koresone Stove"}, {"hh_id": 11012526384304, "elc_aggr_tier": "Tier 0", "l_expenditure": 1200.0, "locality": 1, "c182": null, "i19a": "LPG/cooking gas", "stove": "LPG/Natural Gas Stove"}, {"hh_id": 11016514360802, "elc_aggr_tier": "Tier 3", "l_expenditure": 1000.0, "locality": 0, "c182": null, "i19a": "Kerosene", "stove": "Koresone Stove"}, {"hh_id": 11016516562502, "elc_aggr_tier": "Tier 3", "l_expenditure": 1000.0, "locality": 0, "c182": null, "i19a": "Wood collected", "stove": "3-Stone/Open Fire Stove"}, {"hh_id": 11016516562502, "elc_aggr_tier": "Tier 3", "l_expenditure": 1000.0, "locality": 0, "c182": null, "i19a": "Kerosene", "stove": "NaN"}, {"hh_id": 11016516562602, "elc_aggr_tier": "Tier 3", "l_expenditure": 1000.0, "locality": 0, "c182": null, "i19a": "Wood collected", "stove": "3-Stone/Open Fire Stove"}, {"hh_id": 12006506643201, "elc_aggr_tier": "Tier 4", "l_expenditure": 500.0, "locality": 1, "c182": null, "i19a": "Wood purchased", "stove": "3-Stone/Open Fire Stove"}, {"hh_id": 12014501641601, "elc_aggr_tier": "Tier 3", "l_expenditure": 15000.0, "locality": 1, "c182": null, "i19a": "LPG/cooking gas", "stove": "LPG/Natural Gas Stove"}, {"hh_id": 12018502842102, "elc_aggr_tier": "Tier 3", "l_expenditure": 3000.0, "locality": 0, "c182": null, "i19a": "Wood purchased", "stove": "3-Stone/Open Fire Stove"}, {"hh_id": 12018507800205, "elc_aggr_tier": "Tier 3", "l_expenditure": 3000.0, "locality": 1, "c182": null, "i19a": "Wood purchased", "stove": "3-Stone/Open Fire Stove"}]}}