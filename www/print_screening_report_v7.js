// Oil Palm printable report - v7 portrait
// Builds a clean report-only DOM rather than printing the Shiny card layout.
window.printScreeningReport = function(targetId) {
  var source = document.getElementById(targetId);
  if (!source) {
    alert('The printable report is not available yet. Run the screening first.');
    return;
  }

  var reportWindow = window.open('', '_blank', 'width=900,height=1100');
  if (!reportWindow) {
    alert('The print window was blocked. Allow pop-ups for this local Shiny app and try again.');
    return;
  }

  function cleanText(x) {
    return (x || '').replace(/\s+/g, ' ').trim();
  }

  function cloneNode(selector) {
    var node = source.querySelector(selector);
    return node ? node.cloneNode(true) : null;
  }

  function removeControls(node) {
    if (!node) return;
    node.querySelectorAll(
      '.no-print, .screening-report-actions, button, select, .shiny-input-container, ' +
      '.dataTables_filter, .dataTables_length, .dataTables_paginate, .dataTables_info, ' +
      'a.btn, a.download-button'
    ).forEach(function(el) { el.remove(); });
  }

  function newEl(tag, className, text) {
    var el = document.createElement(tag);
    if (className) el.className = className;
    if (text !== undefined && text !== null) el.textContent = text;
    return el;
  }

  function tableHeaders(table) {
    if (!table) return [];
    return Array.from(table.querySelectorAll('thead th')).map(function(th) {
      return cleanText(th.textContent);
    });
  }

  function headerIndex(headers, label) {
    var target = cleanText(label).toLowerCase();
    for (var i = 0; i < headers.length; i++) {
      if (cleanText(headers[i]).toLowerCase() === target) return i;
    }
    return -1;
  }

  function cellText(row, index) {
    if (index < 0) return '';
    var cells = row.querySelectorAll('td, th');
    return cells[index] ? cleanText(cells[index].textContent) : '';
  }

  function buildSubsetTable(sourceTable, specs, className) {
    if (!sourceTable) return null;
    var headers = tableHeaders(sourceTable);
    var selected = specs.map(function(spec) {
      return {
        index: headerIndex(headers, spec.source),
        label: spec.label || spec.source
      };
    }).filter(function(spec) { return spec.index >= 0; });

    if (selected.length === 0) return null;

    var table = newEl('table', className || 'report-table');
    var thead = document.createElement('thead');
    var hr = document.createElement('tr');
    selected.forEach(function(spec) {
      var th = document.createElement('th');
      th.textContent = spec.label;
      hr.appendChild(th);
    });
    thead.appendChild(hr);
    table.appendChild(thead);

    var tbody = document.createElement('tbody');
    Array.from(sourceTable.querySelectorAll('tbody tr')).forEach(function(row) {
      var tr = document.createElement('tr');
      selected.forEach(function(spec) {
        var td = document.createElement('td');
        td.textContent = cellText(row, spec.index);
        tr.appendChild(td);
      });
      tbody.appendChild(tr);
    });
    table.appendChild(tbody);
    return table;
  }

  function findBaselineScore(sourceTable) {
    if (!sourceTable) return null;
    var headers = tableHeaders(sourceTable);
    var scenarioIdx = headerIndex(headers, 'Scenario');
    var baselineScoreIdx = headerIndex(headers, 'Baseline climate-stress score');
    if (scenarioIdx < 0 || baselineScoreIdx < 0) return null;

    var rows = Array.from(sourceTable.querySelectorAll('tbody tr'));
    for (var i = 0; i < rows.length; i++) {
      if (cellText(rows[i], scenarioIdx).toLowerCase() === 'baseline') {
        return cellText(rows[i], baselineScoreIdx);
      }
    }
    return null;
  }

  function baselineScoreCard(value) {
    if (!value) return null;
    var card = newEl('div', 'print-score-card');
    card.appendChild(newEl('strong', null, 'Baseline climate-stress score'));
    card.appendChild(newEl('div', 'print-score-number', value + ' / 100'));
    card.appendChild(newEl(
      'div',
      'print-score-note',
      'How climate-stressed the AOI is under the 1981-2010 baseline climate.'
    ));
    return card;
  }

  function buildOptionalBlocks(optionalNode) {
    var blocks = [];
    if (!optionalNode) return blocks;
    var sourceTable = optionalNode.querySelector('table');
    if (!sourceTable) return blocks;

    var headers = tableHeaders(sourceTable);
    var dimIdx = headerIndex(headers, 'Dimension');
    var statusIdx = headerIndex(headers, 'Status');
    var indicatorIdx = headerIndex(headers, 'Indicator');
    var unitsIdx = headerIndex(headers, 'Units');
    var scenarioIdx = headerIndex(headers, 'Scenario');
    var periodIdx = headerIndex(headers, 'Period');

    var rows = Array.from(sourceTable.querySelectorAll('tbody tr'));
    var groups = {};
    rows.forEach(function(row) {
      var dimension = dimIdx >= 0 ? cellText(row, dimIdx) : 'Additional dimension';
      if (!groups[dimension]) groups[dimension] = [];
      groups[dimension].push(row);
    });

    Object.keys(groups).forEach(function(dimension) {
      var rowsForDimension = groups[dimension];
      var block = newEl('div', 'optional-dimension-block');
      block.appendChild(newEl('h3', null, dimension));

      var first = rowsForDimension[0];
      var meta = [];
      if (statusIdx >= 0) meta.push('Status: ' + cellText(first, statusIdx));
      if (indicatorIdx >= 0) meta.push('Indicator: ' + cellText(first, indicatorIdx));
      if (unitsIdx >= 0) meta.push('Units: ' + cellText(first, unitsIdx));
      if (meta.length > 0) block.appendChild(newEl('p', 'dimension-meta', meta.join(' | ')));

      var specs = [];
      if (scenarioIdx >= 0) specs.push({source: 'Scenario', label: 'Scenario'});
      if (periodIdx >= 0) specs.push({source: 'Period', label: 'Period'});
      specs = specs.concat([
        {source: 'Baseline', label: 'Baseline'},
        {source: 'Future', label: 'Future'},
        {source: 'Change', label: 'Change'},
        {source: 'Stress worsens', label: 'Worsens'}
      ]);

      var tempTable = sourceTable.cloneNode(true);
      var tempBody = tempTable.querySelector('tbody');
      if (tempBody) {
        tempBody.innerHTML = '';
        rowsForDimension.forEach(function(r) { tempBody.appendChild(r.cloneNode(true)); });
      }
      var table = buildSubsetTable(tempTable, specs, 'report-table optional-table');
      if (table) block.appendChild(table);
      blocks.push(block);
    });

    return blocks;
  }

  var coreTableNode = source.querySelector('#oil_palm_results_table table');
  var coreHeaders = tableHeaders(coreTableNode);
  var isComparison = headerIndex(coreHeaders, 'Scenario') >= 0 &&
                     headerIndex(coreHeaders, 'Future climate-stress score') >= 0;

  var root = newEl('main', 'portrait-report');
  var page1 = newEl('section', 'report-page report-page-1');
  var page2 = newEl('section', 'report-page report-page-2');

  var title = cloneNode('.card-header');
  if (title) page1.appendChild(title);

  var metadata = cloneNode('#oil_palm_print_metadata');
  if (metadata) {
    metadata.classList.remove('print-only');
    metadata.classList.add('report-metadata');
    removeControls(metadata);
    page1.appendChild(metadata);
  }

  var summary = cloneNode('#oil_palm_summary_ui');
  if (summary) {
    removeControls(summary);
    summary.classList.add('report-summary');
    page1.appendChild(summary);
  }

  if (isComparison) {
    var baselineValue = findBaselineScore(coreTableNode);
    var baseCard = baselineScoreCard(baselineValue);
    if (baseCard && summary) {
      var summaryInner = summary.firstElementChild || summary;
      summaryInner.insertBefore(baseCard, summaryInner.firstChild);
    } else if (baseCard) {
      page1.appendChild(baseCard);
    }

    page1.appendChild(newEl('h2', null, 'Scenario comparison summary'));
    var summaryTable = buildSubsetTable(coreTableNode, [
      {source: 'Scenario', label: 'Scenario'},
      {source: 'Period', label: 'Period'},
      {source: 'Future climate-stress score', label: 'Future stress'},
      {source: 'Additional climate-stress-change score', label: 'Stress change'},
      {source: 'Indicators worsening', label: 'Worsening'}
    ], 'report-table summary-comparison-table');
    if (summaryTable) page1.appendChild(summaryTable);

    page2.appendChild(newEl('h2', null, 'Water-stress indicator detail'));
    var detailTable = buildSubsetTable(coreTableNode, [
      {source: 'Scenario', label: 'Scenario'},
      {source: 'Period', label: 'Period'},
      {source: 'Minimum P:PET', label: 'Min P:PET'},
      {source: 'Consecutive months P:PET < 1', label: 'P:PET < 1 months'},
      {source: 'Consecutive dry days', label: 'CDD (days)'}
    ], 'report-table detail-table');
    if (detailTable) page2.appendChild(detailTable);

    var plot = cloneNode('#oil_palm_comparison_plot_ui');
    if (plot && cleanText(plot.textContent) !== 'No comparison values are available for this indicator.') {
      removeControls(plot);
      var plotHeading = newEl('h2', null, 'Comparison graph');
      page2.appendChild(plotHeading);
      plot.classList.add('report-comparison-plot');
      page2.appendChild(plot);
    }

    var detailNote = newEl(
      'p',
      'report-footnote',
      'The summary table is intentionally compact for printing. The CSV download retains the full detailed comparison table.'
    );
    page2.appendChild(detailNote);
  } else {
    page1.appendChild(newEl('h2', null, 'Crop water stress'));
    if (coreTableNode) {
      var singleTable = coreTableNode.cloneNode(true);
      singleTable.classList.add('report-table');
      page1.appendChild(singleTable);
    }
  }

  var generalNotes = Array.from(source.querySelectorAll(':scope > .results-note'));
  if (generalNotes.length > 0) {
    var note = generalNotes[generalNotes.length - 1].cloneNode(true);
    note.classList.add('report-method-note');
    if (isComparison) page2.appendChild(note); else page1.appendChild(note);
  }

  root.appendChild(page1);
  if (isComparison && page2.children.length > 0) root.appendChild(page2);

  var optionalNode = source.querySelector('#oil_palm_optional_results_ui');
  var optionalBlocks = buildOptionalBlocks(optionalNode);
  if (optionalBlocks.length > 0) {
    var page3 = newEl('section', 'report-page report-page-3');
    page3.appendChild(newEl('h2', null, 'Additional dimensions'));
    page3.appendChild(newEl(
      'p',
      'report-intro',
      'Additional dimensions are reported separately and do not change the core crop-water-stress score.'
    ));
    optionalBlocks.forEach(function(block) { page3.appendChild(block); });
    root.appendChild(page3);
  }


  var coastalNode = source.querySelector('#oil_palm_coastal_results_ui');
  if (coastalNode && cleanText(coastalNode.textContent) !== '') {
    var coastalClone = coastalNode.cloneNode(true);
    removeControls(coastalClone);

    var coastalPage;

    if (
      typeof page3 !== 'undefined' &&
      page3
    ) {
      coastalPage = page3;
    } else {
      coastalPage = newEl(
        'section',
        'report-page report-page-coastal'
      );
      root.appendChild(coastalPage);
    }

    coastalPage.appendChild(
      newEl(
        'h2',
        null,
        'Site-specific linked screening'
      )
    );

    coastalClone.classList.add(
      'report-coastal-linked'
    );

    coastalPage.appendChild(
      coastalClone
    );
  }


  var peatFireNode = source.querySelector(
    '#oil_palm_peat_fire_results_ui'
  );

  if (
    peatFireNode &&
    cleanText(peatFireNode.textContent) !== ''
  ) {
    var peatFireClone =
      peatFireNode.cloneNode(true);

    removeControls(
      peatFireClone
    );

    var peatFirePage;

    if (
      typeof page3 !== 'undefined' &&
      page3
    ) {
      peatFirePage = page3;
    } else {
      peatFirePage = newEl(
        'section',
        'report-page report-page-site-specific'
      );

      root.appendChild(
        peatFirePage
      );
    }

    peatFireClone.classList.add(
      'report-peat-fire-linked'
    );

    peatFirePage.appendChild(
      peatFireClone
    );
  }

  var baseHref = document.baseURI;
  var css = `
    @page { size: A4 portrait; margin: 12mm 12mm 13mm 12mm; }
    * { box-sizing: border-box; }
    html, body {
      margin: 0; padding: 0; width: 100%; height: auto; overflow: visible;
      background: #fff; color: #202124;
      font-family: Arial, Helvetica, sans-serif; font-size: 9.5pt; line-height: 1.35;
    }
    body { print-color-adjust: exact; -webkit-print-color-adjust: exact; }
    .portrait-report { width: 100%; max-width: 100%; }
    .report-page {
      width: 100%; max-width: 100%; margin: 0; padding: 0;
      break-after: page; page-break-after: always;
    }
    .report-page:last-child { break-after: auto; page-break-after: auto; }
    .card-header {
      margin: 0 0 7px 0 !important; padding: 0 0 6px 0 !important;
      border: 0 !important; border-bottom: 1px solid #b8b8b8 !important;
      background: #fff !important; font-size: 19pt; font-weight: 700;
    }
    .report-metadata { margin-bottom: 8px; }
    .report-metadata h3 { margin: 0 0 4px 0; font-size: 14pt; }
    .report-metadata p { margin: 2px 0 4px 0; }
    h2 { margin: 12px 0 6px 0; font-size: 13pt; break-after: avoid-page; }
    h3 { margin: 10px 0 5px 0; font-size: 11pt; break-after: avoid-page; }
    p { margin: 4px 0 7px 0; }
    hr { border: 0; border-top: 1px solid #c6c6c6; margin: 7px 0 9px 0; }
    .text-muted, .print-score-note { color: #555 !important; }
    .report-summary > div {
      display: grid !important;
      grid-template-columns: 1fr 1fr !important;
      gap: 7px !important;
      align-items: stretch;
    }
    .report-summary .oil-palm-summary-card {
      grid-column: 1 / -1;
    }
    .oil-palm-summary-card {
      margin: 5px 0 !important; padding: 8px 10px !important;
      border-left: 4px solid #2e7d32 !important; background: #f7faf7 !important;
      break-inside: avoid-page; page-break-inside: avoid;
    }
    .oil-palm-summary-big { color: #1b5e20; font-size: 13.5pt !important; font-weight: 700; }
    .oil-palm-score-card, .print-score-card {
      margin: 0 !important; padding: 8px 9px !important;
      border: 1px solid #c8ddca !important; border-radius: 4px;
      background: #f4f8f4 !important;
      break-inside: avoid-page; page-break-inside: avoid;
    }
    .oil-palm-score-number, .print-score-number {
      margin-top: 2px; color: #1b5e20; font-size: 14pt !important; font-weight: 700;
    }
    .results-note, .report-method-note {
      margin: 7px 0; padding: 6px 8px !important; color: #333 !important;
      background: #fafafa !important; border-left: 3px solid #666 !important;
      border-radius: 0 !important; font-size: 8.8pt;
      break-inside: avoid-page; page-break-inside: avoid;
    }
    .report-table {
      width: 100% !important; max-width: 100% !important;
      border-collapse: collapse !important; table-layout: fixed !important;
      margin: 4px 0 9px 0; font-size: 8.2pt !important;
    }
    .report-table thead { display: table-header-group; }
    .report-table tbody { display: table-row-group; }
    .report-table tr { break-inside: avoid-page; page-break-inside: avoid; }
    .report-table th, .report-table td {
      padding: 3.5px 4px !important; border: 1px solid #d4d8dc !important;
      vertical-align: top; white-space: normal !important; overflow-wrap: anywhere;
      word-break: normal; overflow: visible !important; text-overflow: clip !important;
    }
    .report-table th { background: #f0f2f3 !important; font-weight: 700; }
    .summary-comparison-table { font-size: 8.5pt !important; }
    .detail-table { font-size: 8.1pt !important; }
    .optional-table { font-size: 8.2pt !important; }
    .optional-dimension-block {
      margin: 8px 0 12px 0; break-inside: avoid-page; page-break-inside: avoid;
    }
    .dimension-meta { color: #555; font-size: 8.8pt; }
    .report-comparison-plot {
      width: 100% !important; max-width: 100% !important; height: auto !important;
      min-height: 0 !important; overflow: visible !important;
      break-inside: avoid-page; page-break-inside: avoid;
    }
    .report-comparison-plot img,
    .report-comparison-plot svg,
    .report-comparison-plot canvas {
      display: block; max-width: 100% !important; width: auto !important;
      max-height: 108mm !important; height: auto !important; margin: 0 auto;
    }
    .report-footnote, .report-intro { color: #555; font-size: 8.5pt; }
    .print-only { display: block !important; }
    @media print {
      html, body, .portrait-report { width: 100% !important; height: auto !important; }
    }
  `;

  reportWindow.document.open();
  reportWindow.document.write(
    '<!doctype html>' +
    '<html><head><meta charset="utf-8">' +
    '<meta name="viewport" content="width=device-width, initial-scale=1">' +
    '<base href="' + baseHref + '">' +
    '<title>Oil Palm Climate Stress Report</title>' +
    '<style>' + css + '</style></head><body>' +
    root.outerHTML +
    '</body></html>'
  );
  reportWindow.document.close();

  var triggerPrint = function() {
    reportWindow.focus();
    reportWindow.print();
  };

  // Allow the cloned plot image and table layout to settle before printing.
  var attempts = 0;
  var waitForImages = function() {
    attempts += 1;
    var images = Array.from(reportWindow.document.images || []);
    var ready = images.every(function(img) { return img.complete; });
    if (ready || attempts > 12) {
      setTimeout(triggerPrint, 250);
    } else {
      setTimeout(waitForImages, 100);
    }
  };
  setTimeout(waitForImages, 150);
};
