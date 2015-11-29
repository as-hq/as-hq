import _ from 'underscore';

jasmine.DEFAULT_TIMEOUT_INTERVAL = 10000;

describe('backend', () => {
  const Util = require('../src/js/AS/Util');
  const {
    __injectExpect,

    locToExcel,

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

    copy,
    cut,
    undo,
    redo,
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

    setCondFormattingRules,
    makeCondFormattingRuleFontExcel,

    python,
    r,
    ocaml,
    excel,

    evalHeader,
    pythonEvalHeader,

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
    shouldNotHaveProp
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

  describe('login', () => {
  });

  describe('crud', () => {
    beforeAll((done) => {
      _do([
        logP('Initializing...'),
        exec(done)
      ]);
    });

    it('clears a sheet', () => {
    });

    it('creates a new sheet', () => {
    });

    it('creates a new workbook', () => {
    });

    it('gets cells', () => {
    });
  });

  describe('dispatch', () => {
    beforeAll((done) => {
      _do([
        logP('Initializing...'),
        init(),
        logP('Opening sheet...'),
        openSheet(),
        logP('Syncing window...'),
        syncWindow(),
        logP('Set up environment.'),
        exec(done)
      ]);
    });

    beforeEach((done) => {
      _do([
        logP('Clearing sheet...'),
        clear(), // every it() starts with a clear spreadsheet
        logP('Finished preparing.'),
        logP('==========================STARTING TEST=========================='),
        exec(done)
      ]);
    });

    describe('eval', () => {
      describe('python', () => {
        it('should evaluate at all', (done) => {
          _do([
            python('A1', '1 + 1'),
            shouldBe('A1', valueI(2)),
            exec(done)
          ]);
        });

        it('should evaluate two cells, dependent', (done) => {
          _do([
            python('A1', '1 + 1'),
            python('A2', 'A1 + 1'),
            shouldBe('A1', valueI(2)),
            shouldBe('A2', valueI(3)),
            exec(done)
          ]);
        });

        it('should evaluate a range and expand it', (done) => {
          _do([
            python('A1', 'range(10)'),
            _forM_(_.range(10), (i) => {
              return shouldBe(`A${i + 1}`, valueI(i));
            }),
            exec(done)
          ]);
        });

        it('should fail to evaluate a circular dependency', (done) => {
          _do([
            python('A1', '1+1'),
            python('B1', 'A1+1'),
            shouldError(
              python('A1', 'B1')
            ),
            shouldBe('A1', valueI(2)),
            exec(done)
          ]);
        });

        it('should successfully update diamond dependencies', (done) => {
          _do([
            python('A1', '1'),
            python('A2', 'A1'),
            python('A3', 'A1'),
            python('A4', 'A2+A3'),
            python('A1', '10'),
            shouldBe('A4', valueI(20)),
            exec(done)
          ]);
        });

        it('should delete ancestors that are overwritten by ranges', (done) => {
          _do([
            python('A1', '1'),
            python('A2', 'A1'),
            python('A1', 'range(10)'),
            python('A1', '10'),
            shouldBe('A1', valueI(10)),
            exec(done)
          ]);
        });

        xit('should not give a circular dependency in this contrived example', (done) => {
          _do([
            python('A1', '1'),
            python('B1', 'range(A1)'),
            python('B2', 'A1'),
            python('C1', 'B2'),
            python('A1', '2'),
            shouldBe('C1', valueI(1)),
            exec(done)
          ]);
        });

        it('should not give weird floating point rounding problems on parse', (done) => {
          _do([
            python('A1', '0.07'),
            shouldBeExact('A1', valueD(0.07)),
            exec(done)
          ]);
        });

        it('should rollback ancestors set in failed evals', (done) => {
          _do([
            python('A1', '1'),
            python('A2', '=A1+A2'), // should fail and NOT save anything to graph db. (Until we make circular deps not failed evals in which case this must change again)
            excel('A3','=SUM(A2:A2)+1'), // should be 1
            python('A1', 'A3+2'), // if something got saved to graph db, there should be a circular dep error
            shouldBe('A1', valueI(3)),
            exec(done)
          ]);
        });

        it('should fail to evaluate a circular dependency arising from a range cell', (done) => {
          _do([
            python('A5', '5'),
            python('C5', 'A5 + 10'),
            shouldError(
              python('A1', 'range(C5, C5 + 10)')
            ),
            exec(done)
          ]);
        });

        it('range dependencies get updated', (done) => {
          _do([
            python('A1', 'range(2)'),
            python('B2', 'A2 + 1'),
            python('A1', 'range(4,6)'),
            shouldBe('B2', valueI(6)),
            exec(done)
          ]);
        });

        it('sophisticated range dependencies work as expected', (done) => {
          _do([
            python('A1', 'range(102,110)'),
            python('C3', 'range(A3, A3+3)'),
            python('E3', 'range(A3, A3+4)'),
            python('A1', 'range(C3,E5)'),
            shouldBe('A1', valueI(104)),
            shouldError(
              python('A1', 'range(C3,E6)')
            ),
            python('E3', 'range(A3+C4-104,A3+C3-104+4)'),
            shouldBe('A1', valueI(104)),
            shouldError(
              python('A1', 'range(C3,E6)')
            ),
            exec(done)
          ]);
        });

        it('should evaluate to an error when there is one', (done) => {
          _do([
            python('A1', '1 + "a"'),
            shouldBeError('A1'),
            exec(done)
          ]);
        });

        it('should evaluate negative floats', (done) => {
          _do([
            python('A1', '-1.5'),
            python('A2', 'A1+3'),
            shouldBe('A2', valueD(1.5)),
            exec(done)
          ]);
        });

        it('should reference ancestors of dependencies introduced by list cells in round 2 evals', (done) => {
          _do([
            python('B3', '5'),
            excel('C3', '=SUM(A3:B3)'),
            python('A1', 'range(3)'),
            shouldBe('C3', valueI(7)),
            exec(done)
          ]);
        });

        it('should evaluate None correctly', (done) => {
          _do([
            python('A1', '[1,2,3,None]'),
            python('B1', 'A1:A4.reversed()'),
            shouldBe('B1', noValue()),
            exec(done)
          ]);
        });

        xit('plots shit', (done) => {
          _do([
            python('A1', 'import matplotlib.pyplot as plt; plt.plot([1,2,3])'),
            shouldBeImage('A1'),
            python('A1', 'import matplotlib.pyplot as plt; plt.plot([1,2,3]); plt.show()'),
            shouldBeImage('A1'),
            exec(done)
          ]);
        });

        describe('ASIterable', () => {
          describe('1D ranges', () => {
            it('should act like lists when vertical', (done) => {
              _do([
                python('A1', 'range(10)'),
                python('B1', 'A1:A10[2]'),
                shouldBe('B1', valueI(2)),

                exec(done)
              ]);
            });

            it('initialized to strings works', (done) => {
              _do([
                python('A1', '"Hey"'),
                python('A2', '"There"'),
                python('C1', '[len(x) for x in A1:A2]'),
                shouldBe('C2', valueI(5)),
                exec(done)
              ]);
            });
          });

          describe('2D ranges', () => {
            it('can be accesed like 2D lists', (done) => {
              _do([
                python('B2', '5'),
                python('A1', 'B1:D4[1][0]'),
                shouldBe('A1', valueI(5)),
                exec(done)
              ]);
            });

            it('can be iterated over like 2D lists', (done) => {
              _do([
                python('A1', '5'),
                python('A2', '6'),
                python('B1', '7'),
                python('B2', '8'),
                python('C1', '[[x ** 2 for x in y] for y in A1:B2]'),
                shouldBe('D2', valueI(64)),
                exec(done)
              ]);
            });

            it('cannot be summed over with sum', (done) => {
              _do([
                python('A1', '5'),
                python('A2', '6'),
                python('B1', '7'),
                python('B2', '8'),
                python('C1', 'sum(A1:B2)'),
                shouldBeError('C1'),
                exec(done)
              ]);
            });

            it('can be initialized to strings', (done) => {
              _do([
                python('A1', '"Hey"'),
                python('A2', '"There"'),
                python('B1', '"Pretty"'),
                python('B2', '"Boy"'),
                python('C1', '[len(x[1]) for x in A1:B2]'),
                shouldBe('C2', valueI(3)),
                exec(done)
              ]);
            });
          });

          describe('ASIterables initialization', () => {
            it('works over 1D lists', (done) => {
              _do([
                python('A1', 'arr([1, 2, 3])'),
                python('A2', 'A1:A3[1]'),
                shouldBe('A2', valueI(2)),
                exec(done)
              ]);
            });

            it('works over 1D lists of strings', (done) => {
              _do([
                python('A1', 'arr(["howdy", "there", "pardner"])'),
                python('A2', 'A1:A3[1]'),
                shouldBe('A2', valueS("there")),
                exec(done)
              ]);
            });

            it('works over 2D lists', (done) => {
              _do([
                python('A1', 'arr([[1, 2], [3]])'),
                python('A2', 'A1:A3[1]'),
                shouldBe('A2', valueI(3)),
                exec(done)
              ]);
            });

            it('works over 1D lists of strings', (done) => {
              _do([
                python('A1', 'arr(["howdy", "there", "pardner"])'),
                python('B1', 'A1:A3[1]'),
                shouldBe('B1', valueS("there")),
                exec(done)
              ]);
            });

            it('works over 2D lists of strings', (done) => {
              _do([
                python('A1', 'arr([["howdy", "there", "pardner"], ["how", "are", "you?"]])'),
                python('D1', 'A1:B2[1][0]'),
                shouldBe('D1', valueS("how")),
                exec(done)
              ]);
            });

            it('works over numpy arrays', (done) => {
              _do([
                python('A1', 'import numpy as np;\narr(np.array([[1,2],[3,4]]))'),
                python('C1', 'A1:B2[1][0]'),
                shouldBe('C1', valueI(3)),
                exec(done)
              ]);
            });

            it('works over ASIterables', (done) => {
              _do([
                python('A1', 'arr([arr([1,2]),[3,4]])'),
                python('C1', 'A1:B2[1][0]'),
                shouldBe('C1', valueI(3)),
                exec(done)
              ]);
            });

            it('fails over a 3D list', (done) => {
              _do([
                python('A1', 'arr([[[[1]]]])'),
                shouldBeError('A1'),
                exec(done)
              ]);
            });
          });

          describe('Hiding and unhiding', () => {
            it('can be hidden and unhidden', (done) => {
              _do([
                python('A1', '5'), python('A2', '6'), python('A3', '7'),
                python('B1', 'A1:A3.hide()'),
                shouldBeNothing('B2'),
                python('C1', 'B1.unhide()'),
                shouldBe('C2', valueI(6)),
                exec(done)
              ]);
            });

            it('can be operated on while hidden', (done) => {
              _do([
                python('A1', '5'), python('A2', '6'), python('A3', '7'),
                python('B1', 'A1:A3.hide()'),
                python('C1', 'B1.reversed()'),
                shouldBe('C1', valueI(7)),
                exec(done)
              ]);
            });

            it('preserves dimensions upon hiding and unhiding', (done) => {
              _do([
                python('A1', 'hide([[1,2]])'),
                python('A3', 'A1.unhide()'),
                shouldBe('B3', valueI(2)),
                exec(done)
              ]);
            });
          });

          describe('Misc perks', () => {
            it('can be transposed', (done) => {
              _do([
                python('A1', '5'), python('A2', '6'), python('B1', '7'), python('B2', '8'),
                python('C1', 'A1:B2.transpose()'),
                shouldBe('D1', valueI(6)),
                exec(done)
              ]);
            });

            it('can be summed', (done) => {
              _do([
                python('A1', '5'), python('A2', '6'), python('B1', '7'), python('B2', '8'),
                python('C1', 'A1:B2.sum()'),
                shouldBe('C1', valueI(26)),
                exec(done)
              ]);
            });

            it('can be sorted', (done) => {
              _do([
                python('A1', '7'), python('A2', '5'), python('A3', '6'),
                python('C1', 'A1:A3.sorted()'),
                shouldBe('C2', valueI(6)),
                exec(done)
              ]);
            });

            it('can be reversed', (done) => {
              _do([
                python('A1', '7'), python('A2', '5'), python('A3', '6'),
                python('C1', 'A1:A3.reversed()'),
                shouldBe('C3', valueI(7)),
                exec(done)
              ]);
            });

            it('can be appended as a 2D list', (done) => {
              _do([
                python('A1', '[[1,2],[3,4]]'),
                python('A3', 'l = A1:B2\nl.append([5,6])\nl'),
                shouldBe('B5', valueI(6)),
                exec(done)
              ]);
            });

            it('can be sorted and reversed and transposed in succession', (done) => {
              _do([
                python('A1', '7'), python('A2', '5'), python('A3', '6'),
                python('B1', 'A1:A3.sorted().reversed().transpose()'),
                shouldBe('D1', valueI(5)),
                exec(done)
              ]);
            });
          });
        });
      });

      describe('r', () => {
        it('should evaluate at all', (done) => {
          _do([
            r('A1', '1 + 1'),
            shouldBe('A1', valueI(2)),
            exec(done)
          ]);
        });

        it('should evaluate a range and expand it', (done) => {
          _do([
            r('A1', '1:10'),
            _forM_(_.range(10), (i) => {
              return shouldBe(`A${i + 1}`, valueI(i + 1));
            }),
            exec(done)
          ]);
        });

        it('should evaluate a double', (done) => {
          _do([
            r('A1', '1.23'),
            shouldBe('A1', valueD(1.23)),
            exec(done)
          ]);
        });

        it('should evaluate lists correctly', (done) => {
          _do([
            r('A1', 'list(a=1,b=2)'),
            r('C1', '@A1$a'),
            shouldBe('C1', valueI(1)),
            exec(done)
          ]);
        });

        it('should evaluate a symbol correctly', (done) => {
          _do([
            r('A1', 'as.symbol(123)'),
            shouldBe('A1', valueS('123')),
            exec(done)
          ]);
        });

        it('should evaluate list dependencies', (done) => {
          _do([
            r('A1', 'c(1,2,3,4)'),
            r('B1', 'typeof(A4)'),
            shouldBe('B1', valueS('double')),
            r('A1', 'c("a","b","c","d")'),
            shouldBe('B1', valueS('character')),
            exec(done)
          ]);
        });

        it('plots shit', (done) => {
          _do([
            r('A1','qplot(x=\'x\',y=\'y\',data=data.frame(c(1,2)))'),
            shouldBeImage('A1'),
            exec(done)
          ]);
        });
      });

      describe('excelfunctions', () => {
        // This test won't work until double equality is fixed.
        xit('CORREL', (done) => {
            _do([
                excel('A1', 'Data1'),
                excel('A2', '3'),
                excel('A3', '2'),
                excel('A4', '4'),
                excel('A5', '5'),
                excel('A6', '6'),
                excel('A7', 'Formula'),
                excel('B1', 'Data2'),
                excel('B2', '9'),
                excel('B3', '7'),
                excel('B4', '12'),
                excel('B5', '15'),
                excel('B6', '17'),
                excel('B7', 'Description'),
                excel('B8', 'Correlation coefficient of the two data sets in columns A and B.'),
                excel('C7', 'Result'),
                excel('A8', '=CORREL(A2:A6,B2:B6)'),

                shouldBe('A8', valueD(0.997054486)),

                exec(done)
            ]);
        });
        it('SUM', (done) => {
          _do([
            excel('A1', '-5'),
            excel('A2', '15'),
            excel('A3', '20'),
            excel('A4', '5'),
            excel('A5', 'TRUE'),
            excel('B1', '=SUM(A1,A2)'),
            excel('B2', '=SUM(A2:A4,15)'),
            excel('B2', '=SUM(A2:A5,15)'),
            excel('B3', '=SUM("5", 15, TRUE)'),
            excel('B4', '=SUM(A5,A6, 2)'),
            shouldBe('B1', valueI(10)),
            shouldBe('B2', valueI(55)),
            shouldBe('B3', valueI(21)),
            shouldBe('B4', valueI(2)),
            exec(done)
          ]);
        });
        it('COVAR', (done) => {
          _do([
              excel('A1', 'Data1'),
              excel('A2', '3'),
              excel('A3', '2'),
              excel('A4', '4'),
              excel('A5', '5'),
              excel('A6', '6'),
              excel('A7', 'Formula'),
              excel('A8', '=COVAR(A2:A6, B2:B6)'),
              excel('B1', 'Data2'),
              excel('B2', '9'),
              excel('B3', '7'),
              excel('B4', '12'),
              excel('B5', '15'),
              excel('B6', '17'),
              excel('B7', 'Description'),
              excel('B8', 'Covariance, the average of the products of deviations for each data point pair above.'),
              excel('C7', 'Result'),
              shouldBe('A8', valueD(5.2)),
              exec(done)
          ]);
        });
        xit('MATCH', (done) => {
            _do([
                excel('A1', 'Product'),
                excel('A2', 'Bananas'),
                excel('A3', 'Oranges'),
                excel('A4', 'Apples'),
                excel('A5', 'Pears'),
                excel('A6', 'Formula'),
                excel('A7', '=MATCH(39,B2:B5,1)'),
                excel('A8', '=MATCH(41,B2:B5,0)'),
                excel('A9', '=MATCH(40,B2:B5,-1)'),
                excel('B1', 'Count'),
                excel('B2', '25'),
                excel('B3', '38'),
                excel('B4', '40'),
                excel('B5', '41'),
                excel('B6', 'Description'),
                excel('B7', 'Because there is not an exact match, the position of the next lowest value (38) in the range B2:B5 is returned.'),
                excel('B8', 'The position of the value 41 in the range B2:B5.'),
                excel('B9', 'Returns an error because the values in the range B2:B5 are not in descending order.'),
                excel('C6', 'Result'),

                shouldBe('A7', valueI(2)),
                shouldBe('A8', valueI(4)),
                shouldBe('A9', valueS('#N/A')),

                exec(done)
            ]);
        });
      });

      describe('excel', () => {
        it('should evaluate sums', (done) => {
          _do([
            python('A1', 'range(10)'),
            excel('B1', '=A1+A2'),
            shouldBe('B1', valueI(1)),
            exec(done)
          ]);
        });

        it('should treat blanks as zeroes for arithmetic operations', (done) => {
          _do([
            python('A1', '5'),
            excel('B1', '=A2+A3'),
            shouldBe('B1', valueI(0)),

            excel('B2', '=A1+A3'),
            shouldBe('B2', valueI(5)),

            excel('B3', '=A1*A2'),
            shouldBe('B3', valueI(0)),
            exec(done)
          ]);
        });

        it('should evaluate a literal', (done) => {
          _do([
            excel('A1', '1'),
            shouldBe('A1', valueI(1)),
            exec(done)
          ]);
        });

        it('should evaluate a string literal verbatim', (done) => {
          _do([
            excel('A1', '"hello"hello"hello"'),
            shouldBe('A1', valueS("\"hello\"hello\"hello\"")),
            exec(done)
          ]);
        });

        it('should include quotes in string literal with quotes', (done) => {
          _do([
            excel('A1', '"hello"'),
            shouldBe('A1', valueS("\"hello\"")),
            exec(done)
          ]);
        });

        it('should evaluate entire expression', (done) => {
          _do([
            excel('A1', '=SUM(1,2)ASDF"sadf'), //and not just match =SUM(1,2) and equal 3
            shouldBeError('A1'),
            exec(done)
          ]);
        });

        it('should evaluate nested formulas', (done) => {
          _do([
            excel('A1', '=SUM(1,SUM(2,3))'),
            shouldBe('A1', valueI(6)),
            exec(done)
          ]);
        });

        it('should recognize functions no matter how they are capitalized', (done) => {
          _do([
            excel('A1', '1'),
            excel('A2', '2'),
            excel('A3', '=sUm(A1,A2)'),
            shouldBe('A3', valueI(3)),
            exec(done)
          ]);
        });

        it('should recognize true and false no matter how they are capitalized', (done) => {
          _do([
            excel('A1', 'TrUe'),
            excel('A2', 'false'),
            shouldBe('A1', valueB(true)),
            shouldBe('A2', valueB(false)),
            exec(done)
          ]);
        });

        it('should recognize emptry strings', (done) => {
          _do([
            excel('A1', '   '),
            shouldBe('A1', valueS('   ')),
            exec(done)
          ]);
        });

        it('recognizes - and + prefix operators', (done) => {
          _do([
            excel('A1', '=++--+-2'),
            shouldBe('A1', valueI(-2)),
            exec(done)
          ]);
        });

        describe('abs', () => {
          it('should evaluate', (done) => {
            _do([
              python('A1', 'range(10)'),
              excel('B1', '=abs(A2)'),
              shouldBe('B1', valueI(1)),
              exec(done)
            ]);
          });

          it('should scalarize', (done) => {
            _do([
              python('A1', 'range(10)'),
              excel('B1', '=abs(A$1:A$10)'),
              copy('B1', 'B2:B10'),
              shouldBeL(
                _.range(10).map((i) => `B${i + 1}`),
                _.range(10).map(valueI)
              ),
              exec(done)
            ]);
          });
        });

        describe('equals', () => {
          it('should eval 1=1', (done) => {
            _do([
              python('A1', '1'),
              excel('B1', '=A1=1'),
              shouldBe('B1', valueB(true)),
              exec(done)
            ]);
          });

          it('should array-ize', (done) => {
            _do([
              python('A1', 'range(2)'),
              excel('B1', '{=A1:A2=1}'),
              shouldBeL(['B1', 'B2'], [false, true].map(valueB)),
              exec(done)
            ]);
          });
        });

        describe('exponentiation', () => {
          it('should evaluate integer exponentiation', (done) => {
            _do([
              python('A1', '10'),
              excel('B1', '=A1^2'),
              shouldBe('B1', valueI(100)),
              exec(done)
            ]);
          });

          it('should not raise 0 to the 0', (done) => {
            _do([
              python('A1', '0'),
              excel('B1', '=A1^0'),
              shouldBeError('B1'),
              exec(done)
            ]);
          });

          it('should allow negative numbers to be exponentiated with integers', (done) => {
            _do([
              excel('A1', '=(-2)^(-2)'),
              shouldBe('A1', valueD(0.25)),
              exec(done)
            ]);
          });

          it('should not allow negative numbers to be exponentiated with floats', (done) => {
            _do([
              excel('A1', '=(-2)^(-2.0)'),
              shouldBeError('A1'),
              exec(done)
            ]);
          });
        });

        it('should parse dollars adjacent to operators correctly', (done) => {
          _do([
            excel('A1', '2'),
            excel('A2', '1'),
            excel('A3', '=A1+$A$2'),
            shouldBe('A3', valueI(3)),
            exec(done)
          ]);
        });

        it('should parse floating points correctly', (done) => {
          _do([
            excel('A1', '.25'),
            shouldBe('A1', valueD(0.25)),
            exec(done)
          ]);
        });
      });

      describe('ocaml', () => {

      });
      describe('A:A and  1:1 parsing tests', () => {
        xit('A:A should display ranges properly', (done) => {
          _do([
            python('A1', '[range(10)]'),
            python('B1', 'A:A'), 
            python('C1', 'A1:A'), 
            shouldBe('B9', valueI(8)), 
            shouldBe('B10', valueI(9)), 
            shouldBe('C10', valueI(9)), 
            exec(done)
          ]);
        });
        xit('A2:A should display ranges properly', (done) => {
          _do([
            python('A1', '[range(10)]'),
            python('B1', 'A2:A'), 
            shouldBe('B1', valueI(1)), 
            shouldBe('B9', valueI(9)), 
            exec(done)
          ]);
        });
        xit('A2:B should display ranges properly', (done) => {
          _do([
            python('A1', '[range(10)]'),
            python('B1', 'A:A'),
            python('C1', 'A2:B'), 
            shouldBe('C1', valueI(1)), 
            shouldBe('D1', valueI(1)), 
            shouldBe('C7', valueI(7)), 
            shouldBe('D7', valueI(7)), 
            shouldBe('C8', valueI(8)), 
            shouldBe('D8', valueI(8)), 
            shouldBe('C9', valueI(9)), 
            shouldBe('D9', valueI(9)), 
            exec(done)
          ]);
        });
        xit('A:B should display ranges properly', (done) => {
          _do([
            python('A1', '[range(10)]'),
            python('B1', 'A2:A'), 
            python('B10', '1'), 
            python('C1', 'A:B'), 
            shouldBe('B9', valueI(9)), 
            shouldBe('C1', valueI(2)), 
            shouldBe('D1', valueI(2)), 
            shouldBe('C7', valueI(3)), 
            shouldBe('D7', valueI(3)), 
            shouldBe('C8', valueI(7)), 
            shouldBe('D8', valueI(7)), 
            shouldBe('C10', noValue()), 
            shouldBe('D10', noValue()), 
            exec(done)
          ]);
        });
        xit('1:1 should display ranges properly', (done) => {
          _do([
            python('A1', '[range(10)]'),
            python('B1', '1:1'),
            shouldBe('B1', valueI(0)),
            shouldBe('B2', noValue()),
            exec(done)
          ]);
        });
        xit('Deleting ranges should work with A:A parsing', (done) => {
          _do([
            python('A1', '[range(10)]'),
            delete_('A1'),
            delete_('A3'),
            delete_('A4'),
            delete_('A5'),
            delete_('A8'),
            delete_('A9'),
            python('B1', 'A:A'), 
            shouldBe('B1', noValue()), 
            shouldBe('B2', valueI(1)), 
            shouldBe('B3', noValue()), 
            shouldBe('B9', noValue()), 
            shouldBe('B10', valueI(9)), 
            exec(done)
          ]);
        });

        xit('should redo on Ctrl+Y after undo for A:A', (done) => {
          _do([
            python('A1', 'range(10)'),
            python('B1', 'A:A'),
            undo(),
            shouldBe('B1', noValue()),
            shouldBe('B4', noValue()),
            shouldBe('B10', noValue()),
            shouldBe('A1', noValue()),
            exec(done)
          ]);
        });
      });

      describe('row/col insertion, deletion, and swapping', () => {
        describe('row insertion', () => {
          it('should move cells to correct locations', (done) => {
            _do([
              python('A1', '10'), python('A2', '11'), python('A3', '12'),
              insertRow(2),
              shouldBe('A1', valueI(10)),
              shouldBeNothing('A2'),
              shouldBe('A3', valueI(11)),
              shouldBe('A4', valueI(12)),
              exec(done)
            ]);
          });

          it('should shift references appropriately', (done) => {
            _do([
              python('A1', '10'), python('A2', 'A1+1'), python('A3', 'A2+1'), python('A4', 'A3+1'),
              insertRow(2),
              shouldBe('A3', valueI(11)),
              shouldBe('A4', valueI(12)),
              shouldBe('A5', valueI(13)),
              exec(done)
            ]);
          });

          it('should shift range references appropriately', (done) => {
            _do([
              python('A1', '[range(10)]'),
              excel('A2', '=SUM(A1:J1)'),
              insertRow(1),
              shouldBe('A3', valueI(45)),
              exec(done)
            ]);
          });

          xit('should decouple lists', (done) => {
            _do([
              python('A1', 'range(10)'),
              insertRow(3),
              shouldBe('A1', valueI(0)),
              shouldBe('A2', valueI(1)),
              shouldBeNothing('A3'),
              shouldBe('A4', valueI(2)),
              shouldBe('A11', valueI(9)),
              exec(done)
            ]);
          });
        });

        describe('row deletion', () => {
          it('should move cells to correct locations', (done) => {
            _do([
              python('A1', '10'), python('A2', '11'), python('A3', '12'),
              deleteRow(2),
              shouldBe('A1', valueI(10)),
              shouldBe('A2', valueI(12)),
              exec(done)
            ]);
          });

          it('should shift references appropriately', (done) => {
            _do([
              python('A1', '10'), python('A2', 'A1+1'), python('A3', 'A2+1'), python('A4', 'A3+1'),
              deleteRow(2),
              shouldBeError('A2'), // this used to be A3, which had A2+1 in it.
              python('A2', 'A1+5'),
              shouldBe('A3', valueI(16)),
              exec(done)
            ]);
          });

          it('should shift range references appropriately', (done) => {
            _do([
              python('A2', '[range(10)]'),
              excel('A3', '=SUM(A2:J2)'),
              deleteRow(1),
              shouldBe('A2', valueI(45)),
              exec(done)
            ]);
          });
        });

        describe('row drag', () => {
          it('should move cells to correct locations when dragging left-to-right', (done) => {
            _do([
              python('A1', '10'), python('A2', '11'), python('A3', '12'),
              dragRow(1,3),
              shouldBe('A1', valueI(11)),
              shouldBe('A2', valueI(12)),
              shouldBe('A3', valueI(10)),
              exec(done)
            ]);
          });

          it('should move cells to correct locations when dragging right-to-left', (done) => {
            _do([
              python('A1', '10'), python('A2', '11'), python('A3', '12'),
              dragRow(3,1),
              shouldBe('A1', valueI(12)),
              shouldBe('A2', valueI(10)),
              shouldBe('A3', valueI(11)),
              exec(done)
            ]);
          });

          it('should shift references appropriately', (done) => {
            _do([
              python('A1', '10'), python('A2', 'A1+2'), python('A3', 'A2+3'), python('A4', 'A3+4'),
              dragRow(2,4), //1,2,3,4 --> 1,3,4,2
              shouldBe('A2', valueI(15)),
              shouldBe('A3', valueI(19)),
              shouldBe('A4', valueI(12)),
              exec(done)
            ]);
          });

          it('should shift range references appropriately', (done) => {
            _do([
              python('A1', '0'), python('A2', '1'), python('B1', '3'), python('B2', '4'),
              excel('A5', '=SUM(A1:B2)'),
              dragRow(2,3),
              shouldBe('A5', valueI(8)),
              exec(done)
            ]);
          });
        });

        describe('column insertion', () => {
          it('should move cells to correct locations', (done) => {
            _do([
              python('A1', '10'), python('B1', '11'), python('C1', '12'),
              insertCol(2),
              shouldBe('A1', valueI(10)),
              shouldBeNothing('B1'),
              shouldBe('C1', valueI(11)),
              shouldBe('D1', valueI(12)),
              exec(done)
            ]);
          });

          it('should shift references appropriately', (done) => {
            _do([
              python('A1', '10'), python('B1', 'A1+1'), python('C1', 'B1+1'), python('D1', 'C1+1'),
              insertCol(2),
              shouldBe('C1', valueI(11)),
              shouldBe('D1', valueI(12)),
              shouldBe('E1', valueI(13)),
              exec(done)
            ]);
          });

          it('should shift range references appropriately', (done) => {
            _do([
              python('A1', 'range(10)'),
              excel('B1', '=SUM(A1:A10)'),
              insertCol(1),
              shouldBe('C1', valueI(45)),
              exec(done)
            ]);
          });

          xit('should decouple lists', (done) => {
            _do([
              python('A1', '[range(10)]'),
              insertCol(3),
              shouldBe('A1', valueI(0)),
              shouldBe('B1', valueI(1)),
              shouldBeNothing('C1'),
              shouldBe('D1', valueI(2)),
              shouldBe('K1', valueI(9)),
              exec(done)
            ]);
          });
        });

        describe('column deletion', () => {
          it('should move cells to correct locations', (done) => {
            _do([
              python('A1', '10'), python('B1', '11'), python('C1', '12'),
              deleteCol(2),
              shouldBe('A1', valueI(10)),
              shouldBe('B1', valueI(12)),
              exec(done)
            ]);
          });

          it('should shift references appropriately', (done) => {
            _do([
              python('A1', '10'), python('B1', 'A1+1'), python('C1', 'B1+1'), python('D1', 'C1+1'),
              deleteCol(2),
              shouldBeError('B1'), // this used to be A3, which had A2+1 in it.
              python('B1', 'A1+5'),
              shouldBe('C1', valueI(16)),
              exec(done)
            ]);
          });

          it('should shift range references appropriately', (done) => {
            _do([
              python('B1', 'range(10)'),
              excel('C1', '=SUM(B1:B10)'),
              deleteCol(1),
              shouldBe('B1', valueI(45)),
              exec(done)
            ]);
          });
        });

        describe('column drag', () => {
          it('should move cells to correct locations when dragging up-to-down', (done) => {
            _do([
              python('A1', '10'), python('B1', '11'), python('C1', '12'),
              dragCol(1,3),
              shouldBe('A1', valueI(11)),
              shouldBe('B1', valueI(12)),
              shouldBe('C1', valueI(10)),
              exec(done)
            ]);
          });

          it('should move cells to correct locations when dragging down-to-up', (done) => {
            _do([
              python('A1', '10'), python('B1', '11'), python('C1', '12'),
              dragCol(3, 1),
              shouldBe('A1', valueI(12)),
              shouldBe('B1', valueI(10)),
              shouldBe('C1', valueI(11)),
              exec(done)
            ]);
          });

          it('should shift references appropriately', (done) => {
            _do([
              python('A1', '10'), python('B1', 'A1+2'), python('C1', 'B1+3'), python('D1', 'C1+4'),
              dragCol(2,4), //1,2,3,4 --> 1,3,4,2
              shouldBe('B1', valueI(15)),
              shouldBe('C1', valueI(19)),
              shouldBe('D1', valueI(12)),
              exec(done)
            ]);
          });

          it('should shift references appropriately in succession', (done) => {
            _do([
              python('A1', '10'),
              python('B1', 'A1'),
              dragCol(1, 2),
              dragCol(1, 2),
              expressionShouldBe('B1', 'A1'),
              exec(done)
            ]);
          });

          it('should shift range references appropriately', (done) => {
            _do([
              python('A1', '0'), python('A2', '1'), python('B1', '3'), python('B2', '4'),
              excel('E1', '=SUM(A1:B2)'),
              dragCol(2,3),
              shouldBe('E1', valueI(8)),
              exec(done)
            ]);
          });
        });
      });

      describe('general', () => {
        it('should do multi language eval', (done) => {
          _do([
            python('A1', '10'),
            r('B1', '1:A1'),
            _forM_(_.range(10), (i) => {
              return shouldBe(`B${i + 1}`, valueI(i + 1));
            }),
            exec(done)
          ]);
        });

        it('should shrink a range based on a dependency', (done) => {
          _do([
            python('A1', '10'),
            python('B1', 'range(A1)'),
            _forM_(_.range(10), (i) => {
              return shouldBe(`B${i + 1}`, valueI(i));
            }),
            python('A1', '1'),
            shouldBeNothing('B2'),
            exec(done)
          ]);
        });
      });
    });

    describe('repl eval', () => {
    });

    describe('eval header', () => {
      it('lets you declare global variables', (done) => {
        _do([
          pythonEvalHeader('a=1'),
          python('A1', 'a+1'),
          shouldBe('A1', valueI(2)),
          exec(done)
        ]);
      });

      it('lets you declare global functions', (done) => {
        _do([
          pythonEvalHeader('def sq(x):\n\treturn x**2'),
          python('A1', 'sq(2)'),
          shouldBe('A1', valueI(4)),
          exec(done)
        ]);
      });

      it('lets you make imports', (done) => {
        _do([
          pythonEvalHeader('import numpy as np'),
          python('A1', 'np.array([[1,2],[3,4]]).tolist()'),
          shouldBe('B1', valueI(2)),
          exec(done)
        ]);
      });
    });

    describe('cell transforms', () => {
      describe('copy/paste', () => {
        it('should copy and paste', (done) => {
          _do([
            python('A1', '1'),
            copy('A1', 'A2'),
            shouldBe('A2', valueI(1)),
            exec(done)
          ]);
        });

        it('should copy and paste a reference', (done) => {
          _do([
            python('A1', '1'),
            python('A2', '2'),
            python('B1', 'A1'),
            copy('B1', 'B2'),
            shouldBe('B2', valueI(2)),
            exec(done)
          ]);
        });

        it('should handle $A1 references', (done) => {
          _do([
            python('A1', '1'),
            python('B1', '$A1'),
            copy('B1', 'C1'),
            shouldBe('C1', valueI(1)),
            exec(done)
          ]);
        });

        it('should copy and paste a range reference', (done) => {
          _do([
            python('A1', 'range(10)'),
            python('B1', '[x ** 2 for x in range(10)]'),
            python('C1', 'A1:A10.sum()'),
            copy('C1', 'D1'),
            shouldBe('D1', valueI(285)),
            exec(done)
          ]);
        });

        it('should re-evaluate copy/paste selections composed of only list heads', (done) => {
          _do([
            python('A1', '"John Smith"'),
            python('A2', '"Alex Zhu"'),
            python('A3', '"Bob Ghandi"'),
            python('B1', '[A1.split()]'),
            copy('B1', 'B2:B3'),
            shouldBe('C2', valueS("Zhu")),
            shouldBe('C3', valueS("Ghandi")),
            exec(done)
          ]);
        });

        it('should tessellate a range', (done) => {
          let cs = {
            '00': 0,
            '10': 1,
            '01': 2,
            '11': 3
          };

          _do([
            python('A1', '[[0,1],[2,3]]'),
            copy('A1:B2', 'C1:F4'),
            _forM_(_.range(4), (col) => {
              return _forM_(_.range(4), (row) => {
                return shouldBe(
                  locToExcel({ tl: {col: col + 3, row: row + 1},
                               br: {col: col + 3, row: row + 1}}),
                  valueI(cs[`${col % 2}${row % 2}`])
                );
              });
            }),
            exec(done)
          ]);
        });

        it('should copy a cell down', (done) => {
          _do([
            python('A1', 'range(10)'),
            python('B1', 'A1*2'),
            copy('B1', 'B2:B10'),
            _forM_(fromToInclusive(2, 10), (i) => {
              return shouldBe(`B${i}`, valueI((i-1) * 2));
            }),
            exec(done)
          ]);
        });

        it('should trigger eval when a cell is copied into a dependency', (done) => {
          _do([
            python('A1', '1'),
            python('B1', '2'),
            python('C1', 'B1 + 1'),
            shouldBe('C1', valueI(3)),
            copy('A1', 'B1'),
            shouldBe('C1', valueI(2)),
            exec(done)
          ]);
        });

        it('should refuse to copy to create a circular dependency', (done) => {
          _do([
            python('D1', '5'),
            python('C1', 'D1'),
            python('B1', 'A1 + 1'),
            shouldError(
              copy('B1', 'D1')
            ),
            shouldBe('D1', valueI(5)),
            exec(done)
          ]);
        });

        it('should refuse to copy out of bounds', (done) => {
          _do([
            python('A2', 'A1+1'),
            copy('A2', 'A1'),
            shouldBeError('A1'),
            exec(done)
          ]);
        });

        it('should copy expressions with both a list and a dependency to the list', (done) => {
          _do([
            python('A1', 'range(10)'),
            python('B1', 'A1:A10.sum()'),
            copy('A1:B10', 'C1:D10'),
            shouldBe('D1', valueI(45)),
            exec(done)
          ]);
        });

        it('should successfully copy out of bounds expressions', (done) => {
          _do([
            python('A2', 'A1+1'),
            copy('A2', 'A1'),
            copy('A1', 'B1'),
            shouldBeError('B1'),
            exec(done)
          ]);
        });

        it('should not shift quoted excel references', (done) => {
          _do([
            python('B1', '"there"'),
            python('A2', '"A1"+A1'),
            copy('A2','B2'),
            shouldBe('B2', valueS('A1there')),

            python('A2', 'A1+"A1"'),
            copy('A2','B2'),
            shouldBe('B2', valueS('thereA1')),

            exec(done)
          ]);
        });

        it('should not shift quoted excel references with escaped chars', (done) => {
          _do([
            python('B1', '"there"'),

            python('A2', "A1+'A1'+A1+\"A1\"+\"\\\"A1\\\"\""),
            copy('A2','B2'),
            shouldBe('B2', valueS("thereA1thereA1\"A1\"")),
            exec(done)
          ]);
        });

        it('should not shift excel literals', (done) => {
          _do([
            excel('A1', 'A1"A1"'),
            copy('A1','B1'),
            shouldBe('B1', valueS('A1"A1"')),
            exec(done)
          ]);
        });


        it('should successfully copy and paste cells that depend on each other', (done) => {
          _do([
            python('A1', '1'),
            python('A2', 'A1 + 1'),
            python('B1', 'A2 + 1'),
            python('B2', 'A1 + A2 + B1'),
            copy('A1:B2', 'C1:D2'),
            shouldBeL(
              ['C1', 'C2', 'D1', 'D2'],
              [1, 2, 3, 6].map(valueI)
            ),
            exec(done)
          ]);
        });

        it('should copy an entire list without decoupling it', (done) => {
          _do([
            python('A1', 'range(10)'),
            copy('A1:A10', 'B1:B10'),
            expressionShouldBe('B1', 'range(10)'),
            exec(done)
          ]);
        });

        it('should decouple a partial list while copying it', (done) => {
          _do([
            python('A1', 'range(10)'),
            copy('A1:A2', 'B1:B2'),
            expressionShouldBe('B1', '0'),
            exec(done)
          ]);
        });

        it('should copy blank cells', (done) => {
          _do([
            python('A1', '1'),
            python('B2', '2'),
            copy('A1:A2', 'B1:B2'),
            shouldBeNothing('B2'),
            exec(done)
          ]);
        });
      });

      describe('cut/paste', () => {
        it('should cut properly', (done) => {
          _do([
            python('A1', '1 + 1'),
            python('B1', 'A1 + 1'),
            python('A2', '3'),
            python('B2', '4'),
            cut('A1:B2', 'B1:C2'),
            shouldBeNothing('A1'),
            shouldBeNothing('A2'),
            shouldBe('B1', valueI(2)),
            shouldBe('C1', valueI(3)),
            shouldBe('B2', valueI(3)),
            shouldBe('C2', valueI(4)),
            exec(done)
          ]);
        });

        it('should cut properly with blank cells', (done) => {
          _do([
            python('A1', '1 + 1'),
            python('B1', 'A1 + 1'),
            python('A2', '3'),
            python('C2', '5'),
            cut('A1:B2', 'B1:C2'),
            shouldBeNothing('A1'),
            shouldBeNothing('A2'),
            shouldBe('B1', valueI(2)),
            shouldBe('C1', valueI(3)),
            shouldBe('B2', valueI(3)),
            shouldBeNothing('C2'),
            exec(done)
          ]);
        });

        it('should only shift dependencies in the cut region', (done) => {
          _do([
            python('E1', '10'),
            python('A1', '1 + E1'),
            python('B1', 'A1 + 1'),
            python('A2', 'B1+1'),
            cut('A1:B2', 'B1:C2'),
            expressionShouldBe('B1', '1 + E1'),
            exec(done)
          ]);
        });

        it('should only shift dependencies entirely contained in the cut region for ranges', (done) => {
          _do([
            python('A1', '10'), python('A2', '11'), python('A3', '12'),
            python('A4', 'sum(A1:A3)'),
            python('A5', 'sum(A3:A4)'),
            cut('A3:A5', 'B3:B5'),
            expressionShouldBe('B4', 'sum(A1:A3)'),
            expressionShouldBe('B5', 'sum(B3:B4)'),
            exec(done)
          ]);
        });

        it('should shift absolute index references within the cut region', (done) => {
          _do([
            python('A1', '10'), python('A2', '$A$1+5'),
            cut('A1:A2', 'B1:B2'),
            expressionShouldBe('B2', '$B$1+5'),
            exec(done)
          ]);
        });

        it('should shift absolute range references within the cut region', (done) => {
          _do([
            python('A1', '10'), python('A2', '11'), python('A3', '12'),
            python('A4', 'sum($A$1:$A$3)'),
            cut('A1:A4', 'B1:B4'),
            expressionShouldBe('B4', 'sum($B$1:$B$3)'),
            exec(done)
          ]);
        });

        it('should shift index references in descendant cells', (done) => {
          _do([
            python('A1', '10'), python('A2', '11'), python('A3', '$A$1'),
            cut('A1:A2', 'B1:B2'),
            expressionShouldBe('A3', '$B$1'),
            exec(done)
          ]);
        });

        it('should only shift references in range of cut', (done) => {
          _do([
            python('A1', '10'), python('A2', '11'), python('A4', 'sum($A$2:A3)'),
            cut('A1:A2', 'B1:B2'),
            expressionShouldBe('A4', 'sum($A$2:A3)'),
            exec(done)
          ]);
        });

        it('should not re-eval the head of a fat cell', (done) => {
          _do([
            python('A1', 'range(10)'),
            cut('A1', 'B1'),
            shouldBe('B1', valueI(0)),
            shouldBe('A2', valueI(1)),
            exec(done)
          ]);
        });

        it('should cut/paste entire ranges', (done) => {
          _do([
            python('A1', 'range(10)'),
            cut('A1:A10', 'B1:B10'),
            shouldBe('B1', valueI(0)),
            shouldBeNothing('A1', valueI(0)),
            exec(done)
          ]);
        });
      });

      describe('repeat', () => {
        it('should repeat eval on Ctrl+Y', (done) => {
          _do([
            python('A1', '1'),
            repeat('A2:A10', 'A2'),
            shouldBe('A5', valueI(1)),
            exec(done)
          ]);
        });

        it('should repeat copy on Ctrl+Y', (done) => {
          _do([
            python('A2', 'A1+1'),
            python('B1', '1'),
            copy('A2', 'B2'),
            repeat('B3:B10', 'B3'),
            shouldBe('B5', valueI(5)),
            exec(done)
          ]);
        });

        it('should repeat delete on Ctrl+Y', (done) => {
          _do([
            python('A1', 'range(10)'),
            python('B1', '1'),
            delete_('B1'),
            repeat('A1:B10', 'B1'),
            shouldBeNothing('A1'),
            exec(done)
          ]);
        });

        it('should redo on Ctrl+Y after undo', (done) => {
          _do([
            python('A1', 'range(10)'),
            undo(),
            repeat('A69:A69', 'A69'),
            shouldBe('A5', valueI(4)),
            exec(done)
          ]);
        });
      });
    });

    describe('delete', () => {
      it('should replace a decoupled blank cell with a blank cell', (done) => {
        _do([
          python('A1', '[[None, 2],[3,4]]'),
          delete_('A1'),
          shouldBeNothing('A1'),
          exec(done)
        ]);
      })
    });

    describe('toggling props', () => {
      describe('bolding', () => {
        it('should bold blocks of cells at once', (done) => {
          _do([
            python('A1', '1'),
            python('A2', '2'),
            toggleProp('A1:A2', 'Bold'),
            shouldHaveProp('A1', 'Bold'),
            shouldHaveProp('A2', 'Bold'),
            exec(done)
          ]);
        });

        it('should make all cells in range bold if at least one is not', (done) => {
          _do([
            python('A1', '1'),
            python('A2', '2'),
            toggleProp('A1', 'Bold'),
            toggleProp('A1:A2', 'Bold'),
            shouldHaveProp('A1', 'Bold'),
            shouldHaveProp('A2', 'Bold'),
            exec(done)
          ]);
        });

        it('should unbold all cells in range if all are bold', (done) => {
          _do([
            python('A1', '1'),
            python('A2', '2'),
            toggleProp('A1', 'Bold'),
            toggleProp('A1:A2', 'Bold'),
            toggleProp('A1:A2', 'Bold'),
            shouldNotHaveProp('A1', 'Bold'),
            shouldNotHaveProp('A2', 'Bold'),
            exec(done)
          ]);
        });

        it('should bold blank cells', (done) => {
          _do([
            toggleProp('A1', 'Bold'),
            python('A1', '1'),
            shouldHaveProp('A1', 'Bold'),
            exec(done)
          ]);
        });

        it('should stay bold after a delete', (done) => {
          _do([
            python('A1', '1'),
            toggleProp('A1', 'Bold'),
            delete_('A1'),
            shouldHaveProp('A1', 'Bold'),
            exec(done)
          ]);
        });

        it('should not stay bold after a cut', (done) => {
          _do([
            python('A1', '1'),
            toggleProp('A1', 'Bold'),
            cut('A1', 'B1'),
            shouldNotHaveProp('A1', 'Bold'),
            exec(done)
          ]);
        });
      });

      describe('setting props', () => {
        it('should format blocks of cells at once', (done) => {
          _do([
            python('A1', '1'),
            python('A2', '2'),
            setFormat('A1:A2', 'Money'),
            shouldHaveProp('A1', 'ValueFormat', 'Money'),
            shouldHaveProp('A2', 'ValueFormat', 'Money'),
            exec(done)
          ]);
        });

        it('should format blank cells', (done) => {
          _do([
            setFormat('A1', 'Money'),
            python('A1', '1'),
            shouldHaveProp('A1', 'ValueFormat', 'Money'),
            exec(done)
          ]);
        });

        it('should call the API prop setters successfully', (done) => {
          _do([
            python('A1', 'range(10)'),
            setTextColor('A1', 'red'),
            setFillColor('A2', 'blue'),
            setVAlign('A3', 'TopAlign'),
            setHAlign('A4', 'LeftAlign'),
            setFontSize('A5', 20),
            setFontName('A6', 'Comic Sans'),
            setFormat('A7', 'Money'),
            setUrl('A8', 'PLEASE DO NOT CLICK!!!', 'http://meatspin.com'),
            shouldHaveProp('A1', 'TextColor'),
            shouldHaveProp('A2', 'FillColor'),
            shouldHaveProp('A3', 'VAlign'),
            shouldHaveProp('A4', 'HAlign'),
            shouldHaveProp('A5', 'FontSize'),
            shouldHaveProp('A6', 'FontName'),
            shouldHaveProp('A7', 'ValueFormat'),
            shouldHaveProp('A8', 'URL'),
            exec(done)
          ]);
        });
      });
    });

    describe('conditional formatting', () => {
      it('should format cells already present', (done) => {
        _do([
          python('A1', 'range(10)'), 
          setCondFormattingRules([
            makeCondFormattingRuleFontExcel("A1:A10", "Italic", "=A1<6"),
          ]),
          shouldHaveProp('A6', 'Italic'),
          shouldNotHaveProp('A7', 'Italic'),
          exec(done)
        ]);
      });

      it('should format newly added cells', (done) => {
        _do([
          setCondFormattingRules([
            makeCondFormattingRuleFontExcel("A1:A10", "Italic", "=A1>5"),
          ]),
          python('A1', 'range(10)'), 
          shouldHaveProp('A7', 'Italic'),
          shouldNotHaveProp('A6', 'Italic'),
          exec(done)
        ]);
      });

      it('should apply multiple rules simultaneously', (done) => {
        _do([
          python('A1', 'range(10)'), 
          python('B1', 'range(10)'), 
          setCondFormattingRules([
            makeCondFormattingRuleFontExcel("B1:B10", "Bold", "=B1>4"),
            makeCondFormattingRuleFontExcel("A1:B10", "Italic", "=A1>5"),
          ]),
          shouldHaveProp('B10', 'Italic'),
          shouldHaveProp('B10', 'Bold'),
          shouldHaveProp('A10', 'Italic'),
          shouldNotHaveProp('A10', 'Bold'),
          exec(done)
        ]);
      });

      xit('should revert formats when a rule is deleted (1)', (done) => {
        _do([
          python('A1', 'range(10)'), 
          setCondFormattingRules([
            makeCondFormattingRuleFontExcel("A1:A10", "Italic", "=A1<6"),
          ]),
          shouldHaveProp('A6', 'Italic'),
          setCondFormattingRules([]),
          shouldNotHaveProp('A6', 'Italic'),
          exec(done)
        ]);
      });

      xit('should revert formats when a rule is deleted (2)', (done) => {
        _do([
          python('A1', 'range(10)'), 
          toggleProp('A1', 'Italic'),
          setCondFormattingRules([
            makeCondFormattingRuleFontExcel("A1:A10", "Italic", "=A1<6"),
          ]),
          shouldHaveProp('A6', 'Italic'),
          setCondFormattingRules([]),
          shouldHaveProp('A6', 'Italic'),
          exec(done)
        ]);
      });
    });

    describe('vcs', () => {
      describe('undo', () => {
        it('should undo a simple request', (done) => {
          _do([
            python('A1', '10'),
            undo(),
            shouldBeNothing('A1'), // since cell should be clear
            exec(done)
          ]);
        });

        it('should undo a range request', (done) => {
          _do([
            python('A1', 'range(10)'),
            undo(),
            _forM_(_.range(10), (i) => {
              return shouldBeNothing(`A${i + 1}`);
            }),
            exec(done)
          ]);
        });

        it('should undo a dependency cleanly', (done) => {
          _do([
            python('A1', '1+1'),
            python('B1', 'A1+1'),
            python('C1', 'B1+1'),
            undo(),
            python('B1', '4'),
            shouldBe('B1', valueI(4)),
            shouldBeNothing('C1'),
            exec(done)
          ]);
        });

        it('should undo a copy', (done) => {
          _do([
            python('A1', '1 + 1'),
            python('B1', 'A1 + 1'),
            copy('A1:B1', 'C1:D1'),
            undo(),
            _forM_(['C1', 'D1'],
              shouldBeNothing
            ),
            exec(done)
          ]);
        });

        it('should undo a cut', (done) => {
          _do([
            python('A1', '1 + 1'),
            python('B1', 'A1 + 1'),
            cut('A1:B1', 'C1:D1'),
            undo(),
            _forM_(['C1', 'D1'],
              shouldBeNothing
            ),
            shouldBe('A1', valueI(2)),
            shouldBe('B1', valueI(3)),
            exec(done)
          ]);
        });
      });

      describe('redo', () => {
        it('should undo and redo a simple request', (done) => {
          _do([
            python('A1', '1 + 1'),
            undo(),
            redo(),
            shouldBe('A1', valueI(2)),
            exec(done)
          ]);
        });

        it('should undo and redo a range request', (done) => {
          _do([
            python('A1', 'range(10)'),
            undo(),
            redo(),
            _forM_(_.range(10), (i) => {
              return shouldBe(`A${i + 1}`, valueI(i));
            }),
            exec(done)
          ]);
        });

        it('should undo and redo series of dependencies', (done) => {
          _do([
            python('A1', '1 + 1'),
            python('B1', 'A1 + 1'),
            python('C1', 'A1 + B1'),
            undo(),
            redo(),
            undo(), undo(),
            redo(), redo(),
            undo(), undo(), undo(),
            redo(), redo(), redo(),
            shouldBe('A1', valueI(2)),
            shouldBe('B1', valueI(3)),
            shouldBe('C1', valueI(5)),
            exec(done)
          ]);
        });

        it('should undo and redo copy and paste', (done) => {
          _do([
            python('A1', '1 + 1'),
            python('A2', 'A1 + 1'),
            copy('A1:A2', 'B1:B2'),
            undo(),
            redo(),
            shouldBeL(
              ['B1', 'B2'],
              [2, 3].map(valueI)
            ),
            exec(done)
          ]);
        });

        it('should undo and redo cut and paste', (done) => {
          _do([
            python('A1', '1 + 1'),
            python('A2', 'A1 + 1'),
            cut('A1:A2', 'B1:B2'),
            undo(),
            redo(),
            shouldBeL(
              ['B1', 'B2'],
              [2, 3].map(valueI)
            ),
            shouldBeNothing('A1'),
            shouldBeNothing('A2'),
            exec(done)
          ]);
        });
      });

      describe('list cell behavior on copy/paste', () => {
        it('undoing should delete list key and not cause crashes', (done) => {
          // based off a crash that actually happened
          _do([
            python('A1', 'range(10)'),
            undo(),
            python('A1', 'range(10)'),
            undo(),
            redo(),
            copy('A1:A10', 'B1:B10'),
            expressionShouldBe('B1', "range(10)"),
            exec(done)
          ]);
        });
      });
    });

    describe('pointer syntax', () => {
      it('references python lists', (done) => {
        _do([
          python('A1', 'range(3)'),
          python('B1', '@A1.sum()'),
          shouldBe('B1', valueI(3)),
          exec(done)
          ]);
      });

      it('references r lists ', (done) => {
        _do([
          r('A1', 'c(1,2)'),
          r('B1', 'c(3,4)'),
          r('C1', 'union(@A1, @B1)'),
          shouldBeL(['C1', 'C2', 'C3', 'C4'], [1,2,3,4].map(valueI)),
          exec(done)
          ]);
      });

      it('references dataframes', (done) => {
        _do([
          r('A1', 'data.frame(a=c(1,2))'),
          python('C1', '@A1.T'),
          shouldBe('C2', valueS('a')),
          exec(done)
          ]);
      });

      xit('references series', (done) => {
        _do([
          python('A1', 'pd.Series([1,2,3])'),
          r('B1', '@A1'),
          shouldBeL(['B1','B2','B3'], [1,2,3].map(valueI)),
          exec(done)
          ]);
      });

      it('references np matrices', (done) => {
        _do([
          python('A1', 'np.matrix([[1,2],[3,4]])'),
          python('C1', '@A1 * 2'),
          shouldBe('C1', valueI(2)),
          exec(done)
          ]);
      });

      it('embeds dictionaries', (done) => {
        _do([
          python('A1', '{\'a\':1, \'b\':[1,2,3], \'c\': \'SHIT\'}'),
          shouldBeSerialized('A1'),
          python('B1', 'A1[\'c\']'),
          shouldBe('B1', valueS('SHIT')),
          exec(done)
          ]);
      });

      it('python NaNs', (done) => {
        _do([
          python('A1', 'np.nan'),
          shouldBe('A1', valueNaN()),
          exec(done)
          ]);
      });

      it('python Infs', (done) => {
        _do([
          python('A1', 'np.inf'),
          shouldBe('A1', valueInf()),
          exec(done)
          ]);
      });

      xit('converts NaNs', (done) => {
        _do([
          python('A1', 'np.nan'),
          r('B1', 'A1 + 1'),
          shouldBe('B1', valueNaN()),
          r('A2', 'NaN'),
          python('B2', 'A2 + 1'),
          shouldBe('B2', valueNaN()),
          exec(done)
          ]);
      });

      xit('converts Infs', (done) => {
        _do([
          python('A1', 'np.inf'),
          r('B1', 'A1 + 1'),
          shouldBe('B1', valueInf()),
          r('A2', 'Inf'),
          python('B2', 'A2 + 1'),
          shouldBe('B2', valueInf()),
          exec(done)
          ]);
      });

      // KNOWN TO HANG -- fix this when we diagnose the problem better
      xit('should something something something critch bug', (done) => {
        _do([
          python('A1', 'range(10)'),
          python('C1', '@A1'),
          insertCol(1),
          python('A1', '10'),
          shouldBe('A1', valueI(10)),
          exec(done)
        ]);
      });
    });

    describe('dependencies on expanding cells', (done) => {
      xit('throws error on incorrect pointer ref', (done) => {
        _do([
          python('A1', 'range(10)'),
          python('B1', '@A1'),
          python('A1', '10'),
          shouldBeError('B1'),
          exec(done)
          ]);
      });
      it('deletes fat cell heads properly', (done) => {
        _do([
          python('A1', 'range(10)'),
          delete_('A1'),
          shouldBeDecoupled('A2'),
          exec(done)
          ]);
      });
      xit('propagates overwritten expanded cells', (done) => {
        _do([
          python('A1', 'range(10)'),
          python('B1', '@A1'),
          python('A1', 'range(5)'),
          shouldBeNothing('B6'),
          exec(done)
          ]);
      });
      xit('propagates overwritten expanded cells', (done) => {
        _do([
          python('A1', 'range(10)'),
          python('B1', '@A1'),
          python('A1', 'range(5)'),
          shouldBeNothing('B6'),
          exec(done)
          ]);
      });
    });

    describe('arbitrary datatype embedding in python', (done) => {
      it('embeds lists of dicts', (done) => {
        _do([
          python('A1', '[{\'a\': 1}, {\'b\': 2}]'),
          shouldBeSerialized('A2'),
          exec(done)
          ]);
      });
      it('embeds multidimensional lists', (done) => {
        _do([
          python('A1', '[[[1]]]'),
          shouldBeSerialized('A1'),
          python ('B1', 'A1[0][0][0]'),
          shouldBe('B1', valueI(1)),
          exec(done)
          ]);
      });
      it('embeds arbitrary cell-defined objects', (done) => {
        _do([
          python('A1', 'class A(object):\n\tdef __init__(self):\n\t\tself.x = 5\nA()'),
          shouldBeSerialized('A1'),
          exec(done)
          ]);
      });
      it('lets you reference cell-defined objects', (done) => {
        _do([
          python('A1', 'class A(object):\n\tdef __init__(self):\n\t\tself.x = 5\nA()'),
          python('B1', 'A1.x'),
          shouldBe('B1', valueI(5)),
          exec(done)
          ]);
      });
      xit('expands high-dimensional lists when possible', (done) => {
        _do([
          python('A1', '[[[1]], [[2]]]'),
          shouldBeSerialized('A2'),
          exec(done)
          ]);
      });
      it('lets you make numpy arrays with dtype=dict', (done) => {
        _do([
          python('A1', 'np.array([1, {\'a\': 1}])'),
          shouldBeSerialized('A2'),
          python('B1', 'A2[\'a\']'),
          shouldBe('B1', valueI(1)),
          exec(done)
          ]);
      });
      xit('lets you import scikit datasets', (done) => {
        _do([
          python('A1', 'from sklearn import datsets\ndatasets.load_iris()'),
          shouldBeSerialized('A1'),
          exec(done)
          ]);
      });
      xit('references scikit datasets properly', (done) => {
        _do([
          python('A1', 'from sklearn import datasets\ndatasets.load_iris()'),
          python('B1', 'A1.data'),
          shouldBeCoupled('B1'),
          exec(done)
          ]);
      });
    });

    describe('websockets reliability', () => {
      beforeAll(() => {
        API.setUITestMode();
      });

      afterAll(() => {
        API.unsetUITestMode();
      });

      it('restores connections after failure', (done) => {
        _do([
          exec(() => {
            API.withWS((pws) => {
              pws._withNakedWS((ws) => {
                ws.close();
              })
            });
          }),
          blockUntil(() => {
            return API.withWS((pws) => {
              return pws.readyState() === 1;
            });
          }),
          python('A1', '1'),
          shouldBe('A1', valueI(1)),
          exec(done)
        ]);
      });
    });
  });

});
