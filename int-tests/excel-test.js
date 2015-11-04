
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
      describe('excelfunctions', () => {
        it('UPPER', (done) => {
          _do([
            excel('A1', 'Data'),
            excel('A2', 'total'),
            excel('A3', 'Yield'),
            excel('A4', 'Formula'),
            excel('A5', '=UPPER(A2)'),
            excel('A6', '=UPPER(A3)'),
            excel('B4', 'Description'),
            excel('B5', 'Returns all upper case of text in cell A2.'),
            excel('B6', 'Returns all upper case of text in cell A3.'),
            excel('C4', 'Result'),

            shouldBe('A5', valueS('TOTAL')),
            shouldBe('A6', valueS('YIELD')),

            exec(done)
          ]);
        });
        it('TRIM', (done) => {
          _do([
            excel('A1', 'Formula'),
            excel('A2', '=TRIM(" First Quarter Earnings ")'),
            excel('B1', 'Description'),
            excel('B2', 'Removes leading and trailing spaces from the text in the formula (First Quarter Earnings)'),
            excel('C1', 'Result'),

            shouldBe('A2', valueS('First Quarter Earnings')),

            exec(done)
          ]);
        });
        //TODO: DOESN'T WORK
        xit('SUBSTITUTE', (done) => {
          _do([
            excel('A1', 'Data'),
            excel('A2', 'Sales Data'),
            excel('A3', 'Quarter 1, 2008'),
            excel('A4', 'Quarter 1, 2011'),
            excel('A5', 'Formula'),
            excel('A6', '=SUBSTITUTE(A2, "Sales", "Cost")'),
            excel('A7', '=SUBSTITUTE(A3, "1", "2", 1)'),
            excel('A8', '=SUBSTITUTE(A4, "1", "2", 3)'),
            excel('B2', '<'),
            excel('B3', '<'),
            excel('B4', '<'),
            excel('B5', 'Description (Result)'),
            excel('B6', 'Substitutes Cost for Sales (Cost Data)'),
            excel('B7', 'Substitutes first instance of "1" with "2" (Quarter 2, 2008)'),
            excel('B8', 'Substitutes third instance of "1" with "2" (Quarter 1, 2012)'),
            excel('C5', 'Result'),

            shouldBe('A6', valueS('Cost Data')),
            shouldBe('A8', valueS('Quarter 1, 2012')),

            exec(done)
          ]);
        });
        it('REPT', (done) => {
          _do([
            excel('A1', 'Formula'),
            excel('A2', '=REPT("*-", 3)'),
            excel('A3', '=REPT("-",10)'),
            excel('B1', 'Description'),
            excel('B2', 'Displays an asterisk and a dash (*-) 3 times.'),
            excel('B3', 'Displays a dash (-) 10 times.'),
            excel('C1', 'Result'),

            shouldBe('A2', valueS('*-*-*-')),
            shouldBe('A3', valueS('----------')),

            exec(done)
          ]);
        });
        xit('VARPA', (done) => {
          _do([
            excel('A1', 'Strength'),
            excel('A10', '1303'),
            excel('A11', '1299'),
            excel('A12', 'Formula'),
            excel('A13', '=VARPA(A2:A11)'),
            excel('A14', '=VAR(A2:A11)'),
            excel('A2', '1345'),
            excel('A3', '1301'),
            excel('A4', '1368'),
            excel('A5', '1322'),
            excel('A6', '1310'),
            excel('A7', '1370'),
            excel('A8', '1318'),
            excel('A9', '1350'),
            excel('B12', 'Description'),
            excel('B13', 'Variance of breaking strengths for all the tools, assuming that only 10 tools are produced (entire population).'),
            excel('B14', 'This example uses the VAR function, which assumes a sample of the population, and returns a different result.'),
            excel('C12', 'Result'),

            shouldBe('A13', valueD(678.84)),
            shouldBe('A14', valueD(754.27)),

            exec(done)
          ]);
        });
        xit('VARS', (done) => {
          _do([
            excel('A1', 'Strength'),
            excel('A10', '1303'),
            excel('A11', '1299'),
            excel('A12', 'Formula'),
            excel('A13', '=VAR.S(A2:A11)'),
            excel('A14', '=VAR.P(A2:A11)'),
            excel('A2', '1345'),
            excel('A3', '1301'),
            excel('A4', '1368'),
            excel('A5', '1322'),
            excel('A6', '1310'),
            excel('A7', '1370'),
            excel('A8', '1318'),
            excel('A9', '1350'),
            excel('B12', 'Description'),
            excel('B13', 'Variance for the breaking strength of the tools, when the values in A2:A11 represent only a sample of all the data. VAR.S returns a different result than VAR.P, which treats the range of data as the entire population.'),
            excel('B14', 'The variance based on the entire population, using the VAR.P function, returns a different result.'),
            excel('C12', 'Result'),

            shouldBe('A13', valueD(754.27)),
            shouldBe('A14', valueD(678.84)),

            exec(done)
          ]);
        });
        xit('STDEVS', (done) => {
          _do([
            excel('A1', 'Data'),
            excel('A10', '1350'),
            excel('A11', '1303'),
            excel('A12', '1299'),
            excel('A13', 'Formula'),
            excel('A14', '=STDEV.S(A2:A11)'),
            excel('A2', 'Strength'),
            excel('A3', '1345'),
            excel('A4', '1301'),
            excel('A5', '1368'),
            excel('A6', '1322'),
            excel('A7', '1310'),
            excel('A8', '1370'),
            excel('A9', '1318'),
            excel('B13', 'Description'),
            excel('B14', 'Standard deviation of breaking strength.'),
            excel('C13', 'Result'),

            shouldBe('A14', valueD(27.46391572)),

            exec(done)
          ]);
        });

        xit('SLOPE', (done) => {
          _do([
            excel('A1', 'Data'),
            excel('A10', 'Formula'),
            excel('A11', '=SLOPE(A3:A9,B3:B9)'),
            excel('A2', 'Known y'),
            excel('A3', '1/2/1900'),
            excel('A4', '1/3/1900'),
            excel('A5', '1/9/1900'),
            excel('A6', '1/1/1900'),
            excel('A7', '1/8/1900'),
            excel('A8', '1/7/1900'),
            excel('A9', '1/5/1900'),
            excel('B10', 'Description'),
            excel('B11', 'Slope of the linear regression line through the data points in A3:A9 and B3:B9.'),
            excel('B2', 'Known x'),
            excel('B3', '6'),
            excel('B4', '5'),
            excel('B5', '11'),
            excel('B6', '7'),
            excel('B7', '5'),
            excel('B8', '4'),
            excel('B9', '4'),
            excel('C10', 'Result'),

            shouldBe('A11', valueD(0.305556)),

            exec(done)
          ]);
        });
        //TODO: DOESN'T WORK
        it('RANKEQ', (done) => {
          _do([
            excel('A1', 'Data'),
            excel('A10', '=RANK.EQ(A3,A2:A6,1)'),
            excel('A2', '7'),
            excel('A3', '3.5'),
            excel('A4', '3.5'),
            excel('A5', '1'),
            excel('A6', '2'),
            excel('A7', 'Formula'),
            excel('A8', '=RANK.EQ(A2,A2:A6,1)'),
            excel('A9', '=RANK.EQ(A6,A2:A6)'),
            excel('B10', 'Rank of 3.5 in the same list.'),
            excel('B7', 'Description'),
            excel('B8', 'Rank of  7 in the list contained in the range A2:A6. Because the Order argument (1) is a non-zero value, the list is sorted lowest to highest.'),
            excel('B9', 'Rank of 2 in the same list. Because the Order argument is omitted, the list is sorted, by default, highest to lowest.'),
            excel('C7', 'Result'),

            shouldBe('A10', valueI(3)),
            shouldBe('A8', valueI(5)),
            shouldBe('A9', valueI(4)),

            exec(done)
          ]);
        });
        //TODO: DOESN'T WORK
        it('RANKAVG', (done) => {
          _do([
            excel('A1', 'Day'),
            excel('A10', '=RANK.AVG(94,B2:B8)'),
            excel('A2', '7/1/2011'),
            excel('A3', '7/2/2011'),
            excel('A4', '7/3/2011'),
            excel('A5', '7/4/2011'),
            excel('A6', '7/5/2011'),
            excel('A7', '7/6/2011'),
            excel('A8', '7/7/2011'),
            excel('A9', 'Formula'),
            excel('B1', 'Temp (F)'),
            excel('B10', 'Finds the rank (the position) of the value 94 in the cell range B2:B8. In this case, 7/5/11, when the temperature reached 94, was the 4th hottest day of the days listed.'),
            excel('B2', '89'),
            excel('B3', '88'),
            excel('B4', '92'),
            excel('B5', '101'),
            excel('B6', '94'),
            excel('B7', '97'),
            excel('B8', '95'),
            excel('B9', 'Description'),
            excel('C9', 'Result'),

            shouldBe('A10', valueI(4)),

            exec(done)
          ]);
        });
        xit('PEARSON', (done) => {
          _do([
            excel('A1', 'Data'),
            excel('A2', 'Independent values'),
            excel('A3', '9'),
            excel('A4', '7'),
            excel('A5', '5'),
            excel('A6', '3'),
            excel('A7', '1'),
            excel('A8', 'Formula'),
            excel('A9', '=PEARSON(A3:A7,B3:B7)'),
            excel('B2', 'Dependent values'),
            excel('B3', '10'),
            excel('B4', '6'),
            excel('B5', '1'),
            excel('B6', '5'),
            excel('B7', '3'),
            excel('B8', 'Description (Result)'),
            excel('B9', 'Pearson product moment correlation coefficient for the data sets above (0.699379)'),
            excel('C8', 'Result'),

            shouldBe('A9', valueD(0.699379)),

            exec(done)
          ]);
        });
        xit('NORMINV', (done) => {
          _do([
            excel('A1', 'Data'),
            excel('A2', '0.908789'),
            excel('A3', '40'),
            excel('A4', '1.5'),
            excel('A5', 'Formula'),
            excel('A6', '=NORM.INV(A2,A3,A4)'),
            excel('B1', 'Description'),
            excel('B2', 'Probability corresponding to the normal distribution'),
            excel('B3', 'Arithmetic mean of the distribution'),
            excel('B4', 'Standard deviation of the distribution'),
            excel('B5', 'Description'),
            excel('B6', 'Inverse of the normal cumulative distribution for the terms above (42)'),
            excel('C5', 'Result'),

            shouldBe('A6', valueD(42.000002)),

            exec(done)
          ]);
        });
        xit('NORMDIST', (done) => {
          _do([
            excel('A1', 'Data'),
            excel('A2', '42'),
            excel('A3', '40'),
            excel('A4', '1.5'),
            excel('A5', 'Formula'),
            excel('A6', '=NORM.DIST(A2,A3,A4,TRUE)'),
            excel('A7', '=NORM.DIST(A2,A3,A4,FALSE)'),
            excel('B1', 'Description'),
            excel('B2', 'Value for which you want the distribution'),
            excel('B3', 'Arithmetic mean of the distribution'),
            excel('B4', 'Standard deviation of the distribution'),
            excel('B5', 'Description'),
            excel('B6', 'Cumulative distribution function for the terms above'),
            excel('B7', 'Probability mass function for the terms above'),
            excel('C5', 'Result'),

            shouldBe('A6', valueD(0.9087888)),
            shouldBe('A7', valueD(0.10934)),

            exec(done)
          ]);
        });
        it('MODESNGL', (done) => {
          _do([
            excel('A1', 'Data'),
            excel('A2', '5.6'),
            excel('A3', '4'),
            excel('A4', '4'),
            excel('A5', '3'),
            excel('A6', '2'),
            excel('A7', '4'),
            excel('A8', 'Formula'),
            excel('A9', '=MODE.SNGL(A2:A7)'),
            excel('B8', 'Description'),
            excel('B9', 'Mode, or most frequently occurring number above'),
            excel('C8', 'Result'),

            shouldBe('A9', valueI(4)),

            exec(done)
          ]);
        });
        //TODO: Double error.
        xit('MEDIAN', (done) => {
          _do([
            excel('A1', 'Data'),
            excel('A10', '=MEDIAN(A2:A7)'),
            excel('A2', '3'),
            excel('A3', '2'),
            excel('A4', '1'),
            excel('A5', '5'),
            excel('A6', '6'),
            excel('A7', '7'),
            excel('A8', 'Formula'),
            excel('A9', '=MEDIAN(A2:A6)'),
            excel('B10', 'Median of  the 6 numbers in the range A2:A7. Because there are six numbers, the median is the midway point between the third and fourth numbers.'),
            excel('B8', 'Description'),
            excel('B9', 'Median of the 5 numbers in the range A2:A6. Because there are 5 values, the third is the median.'),
            excel('C8', 'Result'),

            shouldBe('A2', valueD(4)),
            shouldBe('A9', valueI(3)),

            exec(done)
          ]);
        });
        it('MIN', (done) => {
          _do([
            excel('A1', 'Data'),
            excel('A2', '10'),
            excel('A3', '7'),
            excel('A4', '9'),
            excel('A5', '27'),
            excel('A6', '2'),
            excel('A7', 'Formula'),
            excel('A8', '=MIN(A2:A6)'),
            excel('A9', '=MIN(A2:A6,0)'),
            excel('B7', 'Description'),
            excel('B8', 'Smallest of the numbers in the range A2:A6.'),
            excel('B9', 'Smallest of the numbers in the range A2:A6 and 0.'),
            excel('C7', 'Result'),

            shouldBe('A8', valueI(2)),
            shouldBe('A9', valueI(0)),

            exec(done)
          ]);
        });
        it('MAX', (done) => {
          _do([
            excel('A1', 'Data'),
            excel('A2', '10'),
            excel('A3', '7'),
            excel('A4', '9'),
            excel('A5', '27'),
            excel('A6', '2'),
            excel('A7', 'Formula'),
            excel('A8', '=MAX(A2:A6)'),
            excel('A9', '=MAX(A2:A6, 30)'),
            excel('B7', 'Description'),
            excel('B8', 'Largest value in the range A2:A6.'),
            excel('B9', 'Largest value in the range A2:A6 and the value 30.'),
            excel('C7', 'Result'),

            shouldBe('A8', valueI(27)),
            shouldBe('A9', valueI(30)),

            exec(done)
          ]);
        });
        it('COUNT', (done) => {
          _do([
            excel('A1', 'Data'),
            excel('A10', '=COUNT(A5:A7)'),
            excel('A11', '=COUNT(A2:A7,2)'),
            excel('A2', '12/8/08'),
            excel('A4', '19'),
            excel('A5', '22.24'),
            excel('A6', 'TRUE'),
            excel('A7', '#DIV/0!'),
            excel('A8', 'Formula'),
            excel('A9', '=COUNT(A2:A7)'),
            excel('B10', 'Counts the number of cells that contain numbers in cells A5 through A7.'),
            excel('B11', 'Counts the number of cells that contain numbers in cells A2 through A7, and the value 2'),
            excel('B8', 'Description'),
            excel('B9', 'Counts the number of cells that contain numbers in cells A2 through A7.'),
            excel('C8', 'Result'),

            shouldBe('A10', valueI(2)),
            shouldBe('A11', valueI(4)),
            shouldBe('A9', valueI(3)),

            exec(done)
          ]);
        });
        it('COUNTIFS', (done) => {
          _do([
            excel('A1', 'Salesperson'),
            excel('A2', 'Davidoski'),
            excel('A3', 'Burke'),
            excel('A4', 'Sundaram'),
            excel('A5', 'Levitan'),
            excel('A6', 'Formula'),
            excel('A7', '=COUNTIFS(B2:D2,"=Yes")'),
            excel('A8', '=COUNTIFS(B2:B5,"=Yes",C2:C5,"=Yes")'),
            excel('A9', '=COUNTIFS(B5:D5,"=Yes",B3:D3,"=Yes")'),
            excel('B1', 'Exceeded Q1 quota'),
            excel('B2', 'Yes'),
            excel('B3', 'Yes'),
            excel('B4', 'Yes'),
            excel('B5', 'No'),
            excel('B6', 'Description'),
            excel('B7', 'Counts how many times Davidoski exceeded a sales quota for periods Q1, Q2, and Q3 (only in Q1).'),
            excel('B8', 'Counts how many salespeople exceeded both their Q1 and Q2 quotas (Burke and Sundaram).'),
            excel('B9', 'Counts how many times Levitan and Burke exceeded the same quota for periods Q1, Q2, and Q3 (only in Q2).'),
            excel('C1', 'Exceeded Q2 quota'),
            excel('C2', 'No'),
            excel('C3', 'Yes'),
            excel('C4', 'Yes'),
            excel('C5', 'Yes'),
            excel('C6', 'Result'),
            excel('D1', 'Exceeded Q3 quota'),
            excel('D2', 'No'),
            excel('D3', 'No'),
            excel('D4', 'Yes'),
            excel('D5', 'Yes'),

            shouldBe('A7', valueI(1)),
            shouldBe('A8', valueI(2)),
            shouldBe('A9', valueI(1)),

            exec(done)
          ]);
        });
        //TODO: THIS BREAKS THINGS
        xit('COUNTIFS2', (done) => {
          _do([
            excel('A1', 'Data'),
            excel('A10', '=COUNTIFS(A2:A7, "<5",B2:B7,"<5/3/2011")'),
            excel('A11', '=COUNTIFS(A2:A7, "<" & A6,B2:B7,"<" & B4)'),
            excel('A2', '1'),
            excel('A3', '2'),
            excel('A4', '3'),
            excel('A5', '4'),
            excel('A6', '5'),
            excel('A7', '6'),
            excel('A8', 'Formula'),
            excel('A9', '=COUNTIFS(A2:A7,"<6",A2:A7,">1")'),
            excel('B10', 'Counts how many rows have numbers that are less than 5 in cells A2 through A7, and also have dates that are are earlier than 5/3/2011 in cells B2 through B7.'),
            excel('B11', 'Same description as the previous example, but using cell references instead of constants in the criteria.'),
            excel('B2', '5/1/2011'),
            excel('B3', '5/2/2011'),
            excel('B4', '5/3/2011'),
            excel('B5', '5/4/2011'),
            excel('B6', '5/5/2011'),
            excel('B7', '5/6/2011'),
            excel('B8', 'Description'),
            excel('B9', 'Counts how many numbers between 1 and 6 (not including 1 and 6) are contained in cells A2 through A7.'),
            excel('C8', 'Result'),

            shouldBe('A10', valueI(0)),
            shouldBe('A11', valueI(2)),
            shouldBe('A9', valueI(4)),

            exec(done)
          ]);
        });
        xit('BINOMDIST', (done) => {
          _do([
            excel('A1', 'Data'),
            excel('A2', '6'),
            excel('A3', '10'),
            excel('A4', '0.5'),
            excel('A5', 'Formula'),
            excel('A6', '=BINOM.DIST(A2,A3,A4,FALSE)'),
            excel('B1', 'Description'),
            excel('B2', 'Number of successes in trials'),
            excel('B3', 'Number of independent trials'),
            excel('B4', 'Probability of success on each trial'),
            excel('B5', 'Description'),
            excel('B6', 'Probability of exactly 6 of 10 trials being successful.'),
            excel('C5', 'Result'),

            shouldBe('A6', valueD(0.2050781)),

            exec(done)
          ]);
        });
              // This test won't work until double equality is fixed.
        xit('AVERAGEIF', (done) => {
          _do([
            excel('A1', 'Property Value'),
            excel('A10', '=AVERAGEIF(A2:A5,">250000",B2:B5)'),
            excel('A2', '100000'),
            excel('A3', '200000'),
            excel('A4', '300000'),
            excel('A5', '400000'),
            excel('A6', 'Formula'),
            excel('A7', '=AVERAGEIF(B2:B5,"<23000")'),
            excel('A8', '=AVERAGEIF(A2:A5,"<250000")'),
            excel('A9', '=AVERAGEIF(A2:A5,"<95000")'),
            excel('B1', 'Commission'),
            excel('B10', 'Average of all commissions with a property value greater than 250000. Two commissions meet this condition, and their total is 49000.'),
            excel('B2', '7000'),
            excel('B3', '14000'),
            excel('B4', '21000'),
            excel('B5', '28000'),
            excel('B6', 'Description'),
            excel('B7', 'Average of all commissions less than 23000. Three of the four commissions meet this condition, and their total is 42000.'),
            excel('B8', 'Average of all property values less than 250000. Two of the four property values meet this condition, and their total is 300000.'),
            excel('B9', 'Average of all property values less than 95000. Because there are 0 property values that meet this condition, the AVERAGEIF function returns the #DIV/0! error because it tries to divide by 0.'),
            excel('C6', 'Result'),

            shouldBe('A10', valueD(24500)),
            shouldBe('A7', valueD(14000)),
            shouldBe('A8', valueD(150000)),
            //TODO: THIS HAS BEEN COMMENTED OUT
            //shouldBe('A9', valueS('#DIV/0!')),

            exec(done)
          ]);
        });
        it('AVERAGEIF', (done) => {
          _do([
            excel('A1', 'Region'),
            excel('A2', 'East'),
            excel('A3', 'West'),
            excel('A4', 'North'),
            excel('A5', 'South (New Office)'),
            excel('A6', 'MidWest'),
            excel('A7', 'Formula'),
            excel('A8', '=AVERAGEIF(A2:A6,"=*West",B2:B6)'),
            excel('A9', '=AVERAGEIF(A2:A6,"<>*(New Office)",B2:B6)'),
            excel('B1', 'Profits (Thousands)'),
            excel('B2', '45678'),
            excel('B3', '23789'),
            excel('B4', '-4789'),
            excel('B5', '0'),
            excel('B6', '9678'),
            excel('B7', 'Description'),
            excel('B8', 'Average of all profits for the West and MidWest regions.'),
            excel('B9', 'Average of all profits for all regions excluding new offices.'),
            excel('C7', 'Result'),

            //TODO: THIS HAS BEEN COMMENTED OUT
            shouldBe('A8', valueD(16733.5)),
            shouldBe('A9', valueI(18589)),

            exec(done)
          ]);
        });
        //TODO: Double Error
        it('AVERAGEIFS', (done) => {
          _do([
            excel('A1', 'Student'),
            excel('A10', '=AVERAGEIFS(C2:C5, C2:C5, ">95")'),
            excel('A11', '=AVERAGEIFS(D2:D5, D2:D5, "<>Incomplete", D2:D5, ">80")'),
            excel('A4', 'Emilio'),
            excel('A5', 'Julie'),
            excel('A6', 'Hans'),
            excel('A7', 'Frederique'),
            excel('A8', 'Formula'),
            excel('A9', '=AVERAGEIFS(B2:B5, B2:B5, ">70", B2:B5, "<90")'),
            excel('B1', 'First'),
            excel('B10', 'Average second quiz grade that is greater than 95 for all students. Because there are no scores greater than 95, #DIV0! is returned.'),
            excel('B11', 'Average final exam grade that is greater than 80 for all students (87.5). The score marked "Incomplete" is not included in the calculation because it is not a numerical value.'),
            excel('B2', 'Quiz'),
            excel('B3', 'Grade'),
            excel('B4', '75'),
            excel('B5', '94'),
            excel('B6', '86'),
            excel('B7', 'Incomplete'),
            excel('B8', 'Description'),
            excel('B9', 'Average first quiz grade that falls between 70 and 90 for all students (80.5). The score marked "Incomplete" is not included in the calculation because it is not a numerical value.'),
            excel('C1', 'Second'),
            excel('C2', 'Quiz'),
            excel('C3', 'Grade'),
            excel('C4', '85'),
            excel('C5', '80'),
            excel('C6', '93'),
            excel('C7', '75'),
            excel('C8', 'Result'),
            excel('D1', 'Final'),
            excel('D2', 'Exam'),
            excel('D3', 'Grade'),
            excel('D4', '87'),
            excel('D5', '88'),
            excel('D6', 'Incomplete'),
            excel('D7', '75'),

            //TODO: THIS HAS BEEN COMMENTED OUT
            // shouldBe('A10', valueS('#DIV/0!')),
            shouldBe('A11', valueD(87.5)),
            shouldBe('A9', valueI(75)),

            exec(done)
          ]);
        });
        xit('AVERAGEIFS', (done) => {
          _do([
            excel('A1', 'Type'),
            excel('A10', '=AVERAGEIFS(B2:B7, C2:C7, "Issaquah", D2:D7, "<=3",E2:E7, "No")'),
            excel('A2', 'Cozy Rambler'),
            excel('A3', 'Snug Bungalow'),
            excel('A4', 'Cool Cape Codder'),
            excel('A5', 'Splendid Split Level'),
            excel('A6', 'Exclusive Tudor'),
            excel('A7', 'Classy Colonial'),
            excel('A8', 'Formula'),
            excel('A9', '=AVERAGEIFS(B2:B7, C2:C7, "Bellevue", D2:D7, ">2",E2:E7, "Yes")'),
            excel('B1', 'Price'),
            excel('B10', 'Average price of a home in Issaquah that has up to 3 bedrooms and no garage'),
            excel('B2', '230000'),
            excel('B3', '197000'),
            excel('B4', '345678'),
            excel('B5', '321900'),
            excel('B6', '450000'),
            excel('B7', '395000'),
            excel('B8', 'Description'),
            excel('B9', 'Average price of a home in Bellevue that has at least 3 bedrooms and a garage'),
            excel('C1', 'Town'),
            excel('C2', 'Issaquah'),
            excel('C3', 'Bellevue'),
            excel('C4', 'Bellevue'),
            excel('C5', 'Issaquah'),
            excel('C6', 'Bellevue'),
            excel('C7', 'Bellevue'),
            excel('C8', 'Result'),
            excel('D1', 'Number of Bedrooms'),
            excel('D2', '3'),
            excel('D3', '2'),
            excel('D4', '4'),
            excel('D5', '2'),
            excel('D6', '5'),
            excel('D7', '4'),
            excel('E1', 'Garage?'),
            excel('E2', 'No'),
            excel('E3', 'Yes'),
            excel('E4', 'Yes'),
            excel('E5', 'Yes'),
            excel('E6', 'Yes'),
            excel('E7', 'No'),

            shouldBe('A10', valueI(230000)),
            shouldBe('A9', valueI(397839)),

            exec(done)
          ]);
        });
        xit('AVERAGE', (done) => {
          _do([
            excel('A1', 'Data'),
            excel('A10', '=AVERAGE(A2:C2)'),
            excel('A2', '10'),
            excel('A3', '7'),
            excel('A4', '9'),
            excel('A5', '27'),
            excel('A6', '2'),
            excel('A7', 'Formula'),
            excel('A8', '=AVERAGE(A2:A6)'),
            excel('A9', '=AVERAGE(A2:A6, 5)'),
            excel('B10', 'Average of the numbers in cells A2 through C2.'),
            excel('B2', '15'),
            excel('B7', 'Description'),
            excel('B8', 'Average of the numbers in cells A2 through A6.'),
            excel('B9', 'Average of the numbers in cells A2 through A6 and the number 5.'),
            excel('C2', '32'),
            excel('C7', 'Result'),

            shouldBe('A10', valueI(19)),
            shouldBe('A8', valueI(11)),
            shouldBe('A9', valueI(10)),

            exec(done)
          ]);
        });
        //TODO: DOUBLE ERROR
        xit('AVEDEV', (done) => {
          _do([
            excel('A1', 'Data'),
            excel('A10', '=AVEDEV(A2:A8)'),
            excel('A2', '4'),
            excel('A3', '5'),
            excel('A4', '6'),
            excel('A5', '7'),
            excel('A6', '5'),
            excel('A7', '4'),
            excel('A8', '3'),
            excel('A9', 'Formula'),
            excel('B1', 'Description'),
            excel('B2', 'Average of the absolute deviations of the numbers in cells A2:A8 from their mean (1.020408)'),
            excel('B9', 'Result'),

            shouldBe('A10', valueD(1.020408)),

            exec(done)
          ]);
        });
        it('SUMSQ', (done) => {
          _do([
            excel('A1', 'Formula'),
            excel('A2', '=SUMSQ(3, 4)'),
            excel('B1', 'Description (Result)'),
            excel('B2', 'Sum of the squares of 3 and 4 (25)'),
            excel('C1', 'Result'),

            shouldBe('A2', valueI(25)),

            exec(done)
          ]);
        });
        it('SUMX2MY2', (done) => {
          _do([
            excel('A1', 'Data'),
            excel('A10', 'Formula'),
            excel('A11', '=SUMX2MY2(A2:A8,B2:B8)'),
            excel('A12', '=SUMX2MY2({2, 3, 9, 1, 8, 7, 5}, {6, 5, 11, 7, 5, 4, 4})'),
            excel('A2', 'First array'),
            excel('A3', '2'),
            excel('A4', '3'),
            excel('A5', '9'),
            excel('A6', '1'),
            excel('A7', '8'),
            excel('A8', '7'),
            excel('A9', '5'),
            excel('B10', 'Description (Result)'),
            excel('B11', 'Sum of the difference of squares of the two arrays above (-55)'),
            excel('B12', 'Sum of the difference of squares of the two arrays constants (-55)'),
            excel('B2', 'Second array'),
            excel('B3', '6'),
            excel('B4', '5'),
            excel('B5', '11'),
            excel('B6', '7'),
            excel('B7', '5'),
            excel('B8', '4'),
            excel('B9', '4'),
            excel('C10', 'Result'),

            shouldBe('A11', valueI(-55)),
            shouldBe('A12', valueI(-55)),

            exec(done)
          ]);
        });
        it('SUMX2PY2', (done) => {
          _do([
            excel('A1', 'Data'),
            excel('A10', 'Formula'),
            excel('A11', '=SUMX2PY2(A3:A9,B3:B9)'),
            excel('A12', '=SUMX2PY2({2,3,9,1,8,7,5}, {6,5,11,7,5,4,4})'),
            excel('A2', 'First array'),
            excel('A3', '2'),
            excel('A4', '3'),
            excel('A5', '9'),
            excel('A6', '1'),
            excel('A7', '8'),
            excel('A8', '7'),
            excel('A9', '5'),
            excel('B10', 'Description (Result)'),
            excel('B11', 'Sum of the sum of squares of the two arrays above (521)'),
            excel('B12', 'Sum of the sum of squares of the two arrays constants (521)'),
            excel('B2', 'Second array'),
            excel('B3', '6'),
            excel('B4', '5'),
            excel('B5', '11'),
            excel('B6', '7'),
            excel('B7', '5'),
            excel('B8', '4'),
            excel('B9', '4'),
            excel('C10', 'Result'),

            shouldBe('A11', valueI(521)),
            shouldBe('A12', valueI(521)),

            exec(done)
          ]);
        });
        it('SUMXMY2', (done) => {
          _do([
            excel('A1', 'Data'),
            excel('A10', 'Formula'),
            excel('A11', '=SUMXMY2(A3:A9,B3:B9)'),
            excel('A12', '=SUMXMY2({2, 3, 9, 1, 8, 7, 5}, {6, 5, 11, 7, 5, 4, 4})'),
            excel('A2', 'First array'),
            excel('A3', '2'),
            excel('A4', '3'),
            excel('A5', '9'),
            excel('A6', '1'),
            excel('A7', '8'),
            excel('A8', '7'),
            excel('A9', '5'),
            excel('B10', 'Description (Result)'),
            excel('B11', 'Sum of squares of differences of the two arrays above (79)'),
            excel('B12', 'Sum of squares of differences of the two arrays constants (79)'),
            excel('B2', 'Second array'),
            excel('B3', '6'),
            excel('B4', '5'),
            excel('B5', '11'),
            excel('B6', '7'),
            excel('B7', '5'),
            excel('B8', '4'),
            excel('B9', '4'),
            excel('C10', 'Result'),

            shouldBe('A11', valueI(79)),
            shouldBe('A12', valueI(79)),

            exec(done)
          ]);
        });
        it('SUMPRODUCT', (done) => {
          _do([
            excel('A1', 'Array 1'),
            excel('A2', '3'),
            excel('A3', '8'),
            excel('A4', '1'),
            excel('A6', 'Formula'),
            excel('A7', '=SUMPRODUCT(A2:B4, D2:E4)'),
            excel('B2', '4'),
            excel('B3', '6'),
            excel('B4', '9'),
            excel('B6', 'Description'),
            excel('B7', 'Multiplies all the components of the two arrays and then adds the products  that is, 3*2 + 4*7 + 8*6 + 6*7 + 1*5 + 9*3 (156)'),
            excel('C6', 'Result'),
            excel('D1', 'Array2'),
            excel('D2', '2'),
            excel('D3', '6'),
            excel('D4', '5'),
            excel('E2', '7'),
            excel('E3', '7'),
            excel('E4', '3'),

            shouldBe('A7', valueI(156)),

            exec(done)
          ]);
        });
        it('SUMIF', (done) => {
          _do([
            excel('A1', 'Category'),
            excel('A10', '=SUMIF(A2:A7,"Vegetables",C2:C7)'),
            excel('A11', '=SUMIF(B2:B7,"*es",C2:C7)'),
            excel('A12', '=SUMIF(A2:A7,"",C2:C7)'),
            excel('A2', 'Vegetables'),
            excel('A3', 'Vegetables'),
            excel('A4', 'Fruits'),
            excel('A6', 'Vegetables'),
            excel('A7', 'Fruits'),
            excel('A8', 'Formula'),
            excel('A9', '=SUMIF(A2:A7,"Fruits",C2:C7)'),
            excel('B1', 'Food'),
            excel('B10', 'Sum of the sales of all foods in the "Vegetables" category.'),
            excel('B11', 'Sum of the sales of all foods that end in "es" (Tomatoes, Oranges, and Apples).'),
            excel('B12', 'Sum of the sales of all foods that do not have a category specified.'),
            excel('B2', 'Tomatoes'),
            excel('B3', 'Celery'),
            excel('B4', 'Oranges'),
            excel('B5', 'Butter'),
            excel('B6', 'Carrots'),
            excel('B7', 'Apples'),
            excel('B8', 'Description'),
            excel('B9', 'Sum of the sales of all foods in the "Fruits" category.'),
            excel('C1', 'Sales'),
            excel('C2', '2300'),
            excel('C3', '5500'),
            excel('C4', '800'),
            excel('C5', '400'),
            excel('C6', '4200'),
            excel('C7', '1200'),
            excel('C8', 'Result'),

            //TODO: THIS TEST WAS MODIFIED FROM DOLLARS TO INTS.
            shouldBe('A10', valueI('12000')),
            shouldBe('A11', valueI('4300')),
            shouldBe('A12', valueI('400')),
            shouldBe('A9', valueI('2000')),

            exec(done)
          ]);
        });
        xit('SUMIF2', (done) => {
          _do([
            excel('A1', 'Property Value'),
            excel('A10', '=SUMIF(A2:A5,">" & C2,B2:B5)'),
            excel('A2', '$                       100,000.00'),
            excel('A3', '$                       200,000.00'),
            excel('A4', '$                       300,000.00'),
            excel('A5', '$                       400,000.00'),
            excel('A6', 'Formula'),
            excel('A7', '=SUMIF(A2:A5,">160000",B2:B5)'),
            excel('A8', '=SUMIF(A2:A5,">160000")'),
            excel('A9', '=SUMIF(A2:A5,300000,B2:B5)'),
            excel('B1', 'Commission'),
            excel('B10', 'Sum of the commissions for property values greater than the value in C2.'),
            excel('B2', '$                                            7,000.00'),
            excel('B3', '$                                          14,000.00'),
            excel('B4', '$                                          21,000.00'),
            excel('B5', '$                                          28,000.00'),
            excel('B6', 'Description'),
            excel('B7', 'Sum of the commissions for property values over 160,000.'),
            excel('B8', 'Sum of the property values over 160,000.'),
            excel('B9', 'Sum of the commissions for property values equal to 300,000.'),
            excel('C1', 'Data'),
            excel('C2', '$ 250,000.00'),
            excel('C6', 'Result'),

            shouldBe('A10', valueS('$   49,000.00')),
            shouldBe('A7', valueS('$   63,000.00')),
            shouldBe('A8', valueS('$ 900,000.00')),
            shouldBe('A9', valueS('$   21,000.00')),

            exec(done)
          ]);
        });
        xit('SQRTPI', (done) => {
          _do([
            excel('A1', 'Formula'),
            excel('A2', '=SQRTPI(1)'),
            excel('A3', '=SQRTPI(2)'),
            excel('B1', 'Description (Result)'),
            excel('B2', 'Square root of pi.'),
            excel('B3', 'Square root of 2 * pi.'),
            excel('C1', 'Result'),

            shouldBe('A2', valueD(1.772454)),
            shouldBe('A3', valueD(2.506628)),

            exec(done)
          ]);
        });
        xit('SUBTOTAL', (done) => {
          _do([
            excel('A1', 'Data'),
            excel('A10', 'The SUBTOTAL function always requires a numeric argument (1 through 11, 101 through 111) as its first argument. This numeric argument is applied to the subtotal of the values (cell ranges, named ranges) that are specified as the arguments that follow.'),
            excel('A2', '120'),
            excel('A3', '10'),
            excel('A4', '150'),
            excel('A5', '23'),
            excel('A6', 'Formula'),
            excel('A7', '=SUBTOTAL(9,A2:A5)'),
            excel('A8', '=SUBTOTAL(1,A2:A5)'),
            excel('A9', 'Notes'),
            excel('B6', 'Description'),
            excel('B7', 'The sum of the subtotal of the cells A2:A5, using 9 as the first argument.'),
            excel('B8', 'The average of the subtotal of the cells A2:A5, using 1 as the first argument.'),
            excel('C6', 'Result'),

            shouldBe('A7', valueI(303)),
            shouldBe('A8', valueD(75.75)),

            exec(done)
          ]);
        });
        xit('SQRT', (done) => {
          _do([
            excel('A1', 'Data'),
            excel('A2', '-16'),
            excel('A3', 'Formula'),
            excel('A4', '=SQRT(16)'),
            excel('A5', '=SQRT(A2)'),
            excel('A6', '=SQRT(ABS(A2))'),
            excel('B3', 'Description'),
            excel('B4', 'Square root of 16.'),
            excel('B5', 'Square root of -16. Because the number is negative, the #NUM! error message is returned.'),
            excel('B6', 'Avoiding the #NUM! error message by first using the ABS function to find the absolute value of -16 and then finding the square root.'),
            excel('C3', 'Result'),

            shouldBe('A4', valueI(4)),
            shouldBe('A5', valueS('#NUM!')),
            shouldBe('A6', valueI(4)),

            exec(done)
          ]);
        });
        it('PRODUCT', (done) => {
          _do([
            excel('A1', 'Data'),
            excel('A2', '5'),
            excel('A3', '15'),
            excel('A4', '30'),
            excel('A5', 'Formula'),
            excel('A6', '=PRODUCT(A2:A4)'),
            excel('A7', '=PRODUCT(A2:A4, 2)'),
            excel('A8', '=A2*A3*A4'),
            excel('B5', 'Description'),
            excel('B6', 'Multiplies the numbers in cells A2 through A4.'),
            excel('B7', 'Multiplies the numbers in cells A2 through A4, and then multiplies that result by 2.'),
            excel('B8', 'Multiplies the numbers in cells A2 through A4 by using mathematical operators instead of the PRODUCT function.'),
            excel('C5', 'Result'),

            shouldBe('A6', valueI(2250)),
            shouldBe('A7', valueI(4500)),
            shouldBe('A8', valueI(2250)),

            exec(done)
          ]);
        });
        it('ABS', (done) => {
          _do([
            excel('A1', 'Data'),
            excel('A2', '-4'),
            excel('A3', 'Formula'),
            excel('A4', '=ABS(2)'),
            excel('A5', '=ABS(-2)'),
            excel('A6', '=ABS(A2)'),
            excel('B3', 'Description'),
            excel('B4', 'Absolute value of 2'),
            excel('B5', 'Absolute value of -2'),
            excel('B6', 'Absolute value of -4'),
            excel('C3', 'Result'),

            shouldBe('A4', valueI(2)),
            shouldBe('A5', valueI(2)),
            shouldBe('A6', valueI(4)),

            exec(done)
          ]);
        });
        it('TRANSPOSE', (done) => {
          _do([
            excel('A1', 'Data'),
            excel('A2', '1'),
            excel('A3', 'Formula'),
            excel('A4', '=TRANSPOSE($A$2:$C$2)'),
            excel('A5', '=TRANSPOSE($A$2:$C$2)'),
            excel('A6', '=TRANSPOSE($A$2:$C$2)'),
            excel('B2', '2'),
            excel('B3', 'Description'),
            excel('B4', 'Value from A2 transposed into a different cell.'),
            excel('B5', 'Value from B2 transposed into a different cell, and below the value from A2 instead of to its right.'),
            excel('B6', 'Value from C2 transposed into a different cell, and below the value from B2 instead of to its right.'),
            excel('C2', '3'),
            excel('C3', 'Live Result'),

            shouldBe('A4', valueI(1)),
            shouldBe('A5', valueI(2)),
            shouldBe('A6', valueI(3)),

            exec(done)
          ]);
        });
        it('ROW', (done) => {
          _do([
            excel('A1', 'Formula'),
            excel('A2', '=ROW()'),
            excel('A3', '=ROW(C10)'),
            excel('B1', 'Description'),
            excel('B2', 'Row in which the formula appears'),
            excel('B3', 'Row of the reference'),
            excel('C1', 'Result'),

            shouldBe('A2', valueI(2)),
            shouldBe('A3', valueI(10)),

            exec(done)
          ]);
        });

        it('OFFSET', (done) => {
          _do([
            excel('A1', 'Formula'),
            excel('A2', '=OFFSET(D3,3,-2,1,1)'),
            excel('A3', '=SUM(OFFSET(D3:F5,3,-2, 3, 3))'),
            excel('A4', '=OFFSET(D3, -3, -3)'),
            excel('B1', 'Description'),
            excel('B2', 'Displays the value in cell B6 (4)'),
            excel('B3', 'Sums the range B6:C8'),
            excel('B4', 'Returns an error, because the reference is to a non-existent range on the worksheet.'),
            excel('B5', 'Data'),
            excel('B6', '4'),
            excel('B7', '8'),
            excel('B8', '3'),
            excel('C1', 'Result'),
            excel('C5', 'Data'),
            excel('C6', '10'),
            excel('C7', '3'),
            excel('C8', '6'),

            shouldBe('A2', valueI(4)),
            shouldBe('A3', valueI(34)),
            //TODO: THIS HAS BEEN COMMENTED OUT
            //shouldBe('A4', valueS('#REF!')),

            exec(done)
          ]);
        });
        //TODO: MATCH BREAKS THINGS
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
            //TODO: THIS HAS BEEN COMMENTED OUT
            //shouldBe('A9', valueS('#N/A')),

            exec(done)
          ]);
        });
        it('INDIRECT', (done) => {
          _do([
            excel('A1', 'Data'),
            excel('A10', '\'=INDIRECT("B"&A5)'),
            excel('A2', 'B2'),
            excel('A3', 'B3'),
            excel('A4', 'George'),
            excel('A5', '5'),
            excel('A6', 'Formula'),
            excel('A7', '\'=INDIRECT(A2)'),
            excel('A8', '\'=INDIRECT(A3)'),
            excel('A9', '\'=INDIRECT(A4)'),
            excel('B10', 'Combines "B" with the value in A5, which is 5. This, in turn, refers to cell B5, which contains  the value 62.'),
            excel('B2', '1.333'),
            excel('B3', '45'),
            excel('B4', '10'),
            excel('B5', '62'),
            excel('B6', 'Description'),
            excel('B7', 'Value of the reference in cell A2. The reference is to cell B2, which contains the value 1.333.'),
            excel('B8', 'Value of the reference in cell A3. The reference is to cell B3, which contains the value 45.'),
            excel('B9', 'Because cell B4 has the defined name "George," the reference to that defined name is to cell B4, which contains the value 10.'),
            excel('C10', '62'),
            excel('C6', 'Result'),
            excel('C7', '1.333'),
            excel('C8', '45'),
            excel('C9', '10'),


            exec(done)
          ]);
        });
        it('COLUMN', (done) => {
          _do([
            excel('A1', 'Formula'),
            excel('A2', '=COLUMN()'),
            excel('A3', '=COLUMN(B6)'),
            excel('B1', 'Description'),
            excel('B2', 'Column in which the formula appears'),
            excel('B3', 'Column number of the reference B6'),
            excel('C1', 'Result'),

            shouldBe('A2', valueI(1)),
            shouldBe('A3', valueI(2)),

            exec(done)
          ]);
        });
        it('ADDRESS', (done) => {
          _do([
            excel('A1', 'Formula'),
            excel('A2', '=ADDRESS(2,3)'),
            excel('A3', '=ADDRESS(2,3,2)'),
            excel('A4', '=ADDRESS(2,3,2,FALSE)'),
            excel('A5', '=ADDRESS(2,3,1,FALSE,"[Book1]Sheet1")'),
            excel('A6', '=ADDRESS(2,3,1,FALSE,"EXCEL SHEET")'),
            excel('B1', 'Description'),
            excel('B2', 'Absolute reference'),
            excel('B3', 'Absolute row; relative column'),
            excel('B4', 'Absolute row; relative column in R1C1 reference style'),
            excel('B5', 'Absolute reference to another workbook and worksheet'),
            excel('B6', 'Absolute reference to another worksheet'),
            excel('C1', 'Result'),

            shouldBe('A2', valueS('$C$2')),
            shouldBe('A3', valueS('C$2')),
            shouldBe('A4', valueS('R2C[3]')),
            shouldBe('A5', valueS('\'[Book1]Sheet1\'!R2C3')),
            shouldBe('A6', valueS('\'EXCEL SHEET\'!R2C3')),

            exec(done)
          ]);
        });
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
        // This test will fail until blank cells  = 0.
        xit ('SUM', (done) => {
          _do([
            excel('A1', '-5'),
            excel('A2', '15'),
            excel('A3', '20'),
            excel('A4', '5'),
            excel('A5', 'TRUE'),
            excel('B1', '=SUM(A1,A2)'),
            excel('B2', '=SUM(A2:A4,15)'),
            excel('B3', '=SUM("5", 15, TRUE)'),
            excel('B4', '=SUM(A5,A6, 2)'),
            shouldBe('B1', valueI(10)),
            shouldBe('B2', valueI(55)),
            shouldbe('B3', valueI(21)),
            shouldbe('B4', valueI(2)),
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
      });
  });
});
