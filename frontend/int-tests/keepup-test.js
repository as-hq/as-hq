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
      
      it ('can eval', (done) => {
        _do([
          python('A1', '=1+1'),
          r('B1', '=1+1'),
          excel('C1', '=1+1'),
          shouldBe('A1', valueI(2)),
          shouldBe('B1', valueI(2)),
          shouldBe('C1', valueI(2)),
          exec(done)
        ]);
      });

    });

  });

});
