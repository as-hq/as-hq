import _ from 'underscore';

jasmine.DEFAULT_TIMEOUT_INTERVAL = 10000;

describe('stress testing', () => {
  const Util = require('../src/js/AS/Util');
  const {
    __injectExpect,

    locToExcel,

    openCurrentSheet,
    openSheet,
    syncWindow,
    init,
    clear,

    repeat,

    insertCol,
    insertRow,
    deleteCol,
    deleteRow,
    dragCol,
    dragRow,
    dragInference,

    copy,
    cut,
    undo,
    redo,
    decouple,
    delete_,

    toggleProp,
    setTextColor,
    setFillColor,
    setVAlign,
    setHAlign,
    setFontSize,
    setFontName,
    setFormat,
    setUrl,

    updateCondFormattingRule,
    removeCondFormattingRule,
    makeCustomCondFormattingFontRuleExcel,
    makeGreaterThanCondFormattingFontRuleExcel,
    makeLessThanCondFormattingFontRuleExcel,
    makeEqualsCondFormattingFontRuleExcel,
    makeGeqCondFormattingFontRuleExcel,
    makeLeqCondFormattingFontRuleExcel,
    makeNotEqualsCondFormattingFontRuleExcel,
    makeIsEmptyCondFormattingFontRuleExcel,
    makeIsNotEmptyCondFormattingFontRuleExcel,
    makeIsBetweenCondFormattingFontRuleExcel,
    makeIsNotBetweenCondFormattingFontRuleExcel,

    makeLambdaRule,

    setColumnWidth,

    python,
    r,
    excel,
    sql,

    evalHeader,
    pythonEvalHeader,
    rEvalHeader,

    valueD,
    valueI,
    valueS,
    valueB,
    noValue,
    valueInf,
    valueNaN,

    shouldError,
    shouldBe,
    shouldBeExact,
    shouldBeL,
    shouldBeError,
    shouldBeNothing,
    shouldBeImage,
    shouldBeSerialized,
    shouldBeDecoupled,
    shouldBeCoupled,
    expressionShouldBe,
    shouldHaveProp,
    shouldNotHaveProp,

    colShouldHaveDimension,
    colShouldNotHaveDimensionProp
  } = require('../src/js/browser-test/exec-api');
  const {
    fromToInclusive,
    logP,
    _do,
    _doDefer,
    _forM_,
    exec,
    blockUntil
  } = require('../src/js/browser-test/exec-monad');

  const API = require('../src/js/actions/ASApiActionCreators');

  beforeAll(() => {
    __injectExpect(expect);
  });

  describe('stress-test-run', () => {
    beforeAll((done) => {
      _do([
        logP('Initializing...'),
        init (),
        logP('Opening sheet...'),
        openSheet(),
        logP('Syncing window...'),
        syncWindow(),
        logP('Set up environment.'),
        exec(done)
      ]);
    });

    describe('basic operations', () => {
      it ('makes ranges and operates on them', (done) => {
        _do([
          python('A1', 'range(10)'),
          python('B1', 'range(10000)'),
          excel('C1', '=sum(A1:A100)'),
          excel('D1', '=sum(A:A)'),
          exec(done)
        ]);
      });

      xit('decouples the ranges just made', (done) => {
        _do([
          python('A1', '\'decoupled\''),
          decouple(),
          python('B1', '\'decoupled\''),
          decouple(),
          exec(done)
        ]);
      });

      it('copies data within the spreadsheet', (done) => {
        _do([
          copy('B1:B100', 'E1'),
          exec(done)
        ]);
      });

      it('samples cells', (done) => {
        _do([
          python('A1', 'random.random()'),
          python('B1', '!{10, A1}'),
          python('C1', '!{100, A1}'),
          python('D1', '!{1000, A1}'),
          exec(done)
        ]);
      });

      it('updates dependencies', (done) => {
        _do([
          python('E1', 'range(int(A1 * 10))'),
          python('A1', 'random.random() * 10'),
          exec(done)
        ]);
      });

      it('deletes all of the data just created', (done) => {
        _do([
          delete_('A1:E100'),
          exec(done)
        ]);
      });
    });

  });

});
