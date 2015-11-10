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
    paste,
    cut,
    undo,
    redo,
    delete_,

    python,
    r,
    ocaml,
    excel,

    valueD,
    valueI,
    valueS,
    valueB,
    noValue,

    shouldError,
    shouldBe,
    shouldBeL,
    shouldBeError,
    shouldBeNothing,
    shouldBeImage,
    expressionShouldBe
  } = require('../src/js/browser-test/exec-api');
  const {
    fromToInclusive,
    logP,
    _do,
    _doDefer,
    _forM_,
    exec
  } = require('../src/js/browser-test/exec-monad');

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
            python('C3', 'A3:B3.sum()'),
            python('A1', 'range(3)'),
            shouldBe('C3', valueI(7)),
            exec(done)
          ]);
        });

        it ('should evaluate None correctly', (done) => {
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

            // No longer supported. (Alex 11/9)
            // it('should act like lists when horizontal', (done) => {
            //   _do([
            //     python('A1', '[range(10)]'),
            //     python('A5', 'A1:D1[2]'),
            //     shouldBe('A5', valueI(2)),

            //     exec(done)
            //   ]);
            // });

            // it('can be iterated over like a 1D list', (done) => {
            //   _do([
            //     python('A1', '[range(10)]'),
            //     python('A2', '[x ** 2 for x in B1:D1]'), // expands to vertical list
            //     shouldBe('A3', valueI(4)),
            //     exec(done)
            //   ]);
            // });

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
            r('B1', 'A1$a'),
            shouldBe('B1', valueI(1)),
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

        xit('plots shit', (done) => {
          _do([
            r('A1','qplot(x=\'x\',y=\'y\',data=data.frame(c(1,2)))'),
            shouldBeImage('A1'),
            exec(done)
          ]);
        });
      });

      describe('excelfunctions', () => {
        // This test won't work until double equality is fixed.
        xit ('CORREL', (done) => {
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
        it ('SUM', (done) => {
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
        it ('COVAR', (done) => {
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
        xit ('MATCH', (done) => {
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

        it ('should treat blanks as zeroes for arithmetic operations', (done) => {
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
          it('should move cells to correct locations', (done) => {
            _do([
              python('A1', '10'), python('A2', '11'), python('A3', '12'),
              dragRow(1,3), 
              shouldBe('A1', valueI(11)),
              shouldBe('A2', valueI(12)), 
              shouldBe('A3', valueI(10)), 
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

        describe('Column drag', () => {
          it('should move cells to correct locations', (done) => {
            _do([
              python('A1', '10'), python('B1', '11'), python('C1', '12'),
              dragCol(1,3), 
              shouldBe('A1', valueI(11)),
              shouldBe('B1', valueI(12)), 
              shouldBe('C1', valueI(10)), 
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
            cut('A1:B2', 'B1:C2'),
            shouldBeNothing('A1'),
            shouldBeNothing('A2'),
            shouldBe('B1', valueI(2)),
            shouldBe('C1', valueI(3)),
            shouldBe('B2', valueI(3)),
            exec(done)
          ]);
        });

        // works on backend but crashes the test
        xit('should not re-eval a non-head list cell with its expression unchanged', (done) => {
          _do([
            python('A1', 'range(10)'),
            python('A2', 'range(10)'),
            shouldBe('A2', valueI(1)),
            exec(done)
          ]);
        });

        xit('should shrink a range based on a dependency', (done) => {
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

    describe('cell transforms', () => {
      describe('copy/cut/paste', () => {
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
  });

});
