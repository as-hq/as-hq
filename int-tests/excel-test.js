import _ from 'underscore';

jasmine.DEFAULT_TIMEOUT_INTERVAL = 10000;

describe('excel', () => {
  const {
    __injectExpect,

    openSheet,
    syncWindow,
    init,
    clear,

    excel,

    valueD,
    valueI,
    valueS,
    valueB,

    shouldBe,
  } = require('../src/js/browser-test/exec-api');
  const {
    logP,
    _do,
    exec
  } = require('../src/js/browser-test/exec-monad');

  beforeAll((done) => {
    __injectExpect(expect);

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

  it('MATCH', (done) => {
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
  it('CHOOSE', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A2', '1st'),
      excel('A3', '2nd'),
      excel('A4', '3rd'),
      excel('A5', 'Finished'),
      excel('A6', 'Formula'),
      excel('A7', '=CHOOSE(2,A2,A3,A4,A5)'),
      excel('A8', '=CHOOSE(4,B2,B3,B4,B5)'),
      excel('A9', '=CHOOSE(3,"Wide",115,"world",8)'),
      excel('B2', 'Nails'),
      excel('B3', 'Screws'),
      excel('B4', 'Nuts'),
      excel('B5', 'Bolts'),
      excel('B6', 'Description'),
      excel('B7', 'Value of the second list argument (value of cell A3)'),
      excel('B8', 'Value of the fourth list argument (value of cell B5)'),
      excel('B9', 'Value of the third list argument'),
      excel('C6', 'Result'),

      shouldBe('A7', valueS('2nd')),
      shouldBe('A8', valueS('Bolts')),
      shouldBe('A9', valueS('world')),

      exec(done)
    ]);
  });
  it('CHOOSE', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A2', '23'),
      excel('A3', '45'),
      excel('A4', '12'),
      excel('A5', '10'),
      excel('A6', 'Formula'),
      excel('A7', '=SUM(A2:CHOOSE(2,A3,A4,A5))'),
      excel('B6', 'Description (Result)'),
      excel('B7', 'Sums the range A2:A4. The CHOOSE function returns A4 as the second part of the range for the SUM function.'),
      excel('C6', 'Result'),

      shouldBe('A7', valueI(80)),

      exec(done)
    ]);
  });
  it('DATE', (done) => {
    _do([
      excel('A1', 'Year'),
      excel('A2', '2011'),
      excel('A3', 'Data'),
      excel('A4', '20111125'),
      excel('A5', 'Formula'),
      excel('A6', '=DATE(A2,B2,C2)'),
      excel('A7', '=DATE(YEAR(TODAY()),12,31)'),
      excel('A8', '=DATE(LEFT(A4,4),MID(A4,5,2), RIGHT(A4,2))'),
      excel('B1', 'Month'),
      excel('B2', '1'),
      excel('B5', 'Description'),
      excel('B6', 'Serial date for the date derived by using cells A2, B2, and C2 as the arguments for the DATE function.'),
      excel('B7', 'Serial date for the last day of the current year.'),
      excel('B8', 'A formula that converts the text string in A4 (20111125) representing a date in a "YYYYMMDD" format to a date.'),
      excel('C1', 'Day'),
      excel('C2', '1'),
      excel('C5', 'Result'),

      shouldBe('A6', valueI(40544)),
      shouldBe('A7', valueI(41274)),
      shouldBe('A8', valueS('11/25/2011')),

      exec(done)
    ]);
  });
  it('DAYS', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A2', '12/31/2011'),
      excel('A3', '1/1/2011'),
      excel('A4', 'Formula'),
      excel('A5', '=DAYS("3/15/11","2/1/11")'),
      excel('A6', '=DAYS(A2,A3)'),
      excel('B4', 'Description'),
      excel('B5', 'Finds the number of days between the end date (3/15/11) and end date (2/1/11). When you enter a date directly in the function, you need to enclose it in quotation marks. Result is 42.'),
      excel('B6', 'Finds the number of days between the end date in A2 and the start date in A3 (364).'),
      excel('C4', 'Result'),

      shouldBe('A5', valueI(42)),
      shouldBe('A6', valueI(364)),

      exec(done)
    ]);
  });
  it('INDEX', (done) => {
    _do([
      excel('A1', 'Fruit'),
      excel('A10', 'Peanuts'),
      excel('A11', 'Walnuts'),
      excel('A12', 'Formula'),
      excel('A13', '=INDEX(A2:C6, 2, 3)'),
      excel('A14', '=INDEX((A1:C6, A8:C11), 2, 2, 2)'),
      excel('A15', '=SUM(INDEX(A1:C11, 0, 3, 1))'),
      excel('A16', '=SUM(B2:INDEX(A2:C6, 5, 2))'),
      excel('A2', 'Apples'),
      excel('A3', 'Bananas'),
      excel('A4', 'Lemons'),
      excel('A5', 'Oranges'),
      excel('A6', 'Pears'),
      excel('A8', 'Almonds'),
      excel('A9', 'Cashews'),
      excel('B1', 'Price'),
      excel('B10', '$1.25'),
      excel('B11', '$1.75'),
      excel('B12', 'Description'),
      excel('B13', 'The intersection of the second row and third column in the range A2:C6, which is the contents of cell C3.'),
      excel('B14', 'The intersection of the second row and second column in the second area of A8:C11, which is the contents of cell B9.'),
      excel('B15', 'The sum of the third column in the first area of the range A1:C11, which is the sum of C1:C6.'),
      excel('B16', 'The sum of the range starting at B2, and ending at the intersection of the fifth row and the second column of the range A2:A6, which is the sum of B2:B6.'),
      excel('B2', '$0.69'),
      excel('B3', '$0.34'),
      excel('B4', '$0.55'),
      excel('B5', '$0.25'),
      excel('B6', '$0.59'),
      excel('B8', '$2.80'),
      excel('B9', '$3.55'),
      excel('C1', 'Count'),
      excel('C10', '20'),
      excel('C11', '12'),
      excel('C12', 'Result'),
      excel('C2', '40'),
      excel('C3', '38'),
      excel('C4', '15'),
      excel('C5', '25'),
      excel('C6', '40'),
      excel('C8', '10'),
      excel('C9', '16'),

      shouldBe('A13', valueI(38)),
      shouldBe('A14', valueD(3.55)),
      shouldBe('A15', valueI(216)),
      shouldBe('A16', valueD(2.42)),

      exec(done)
    ]);
  });
  it('BETADIST', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A2', '2'),
      excel('A3', '8'),
      excel('A4', '10'),
      excel('A5', '1'),
      excel('A6', '3'),
      excel('A7', 'Formula'),
      excel('A8', '=BETADIST(A2,A3,A4,A5,A6)'),
      excel('B1', 'Description'),
      excel('B2', 'Value at which to evaluate the function'),
      excel('B3', 'Parameter of the distribution'),
      excel('B4', 'Parameter of the distribution'),
      excel('B5', 'Lower bound'),
      excel('B6', 'Upper bound'),
      excel('B7', 'Description'),
      excel('B8', 'Cumulative beta probability density function, for the above parameters'),
      excel('C7', 'Result'),

      shouldBe('A8', valueD(0.6854706)),

      exec(done)
    ]);
  });
  it('BETAINV', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A2', '0.685470581'),
      excel('A3', '8'),
      excel('A4', '10'),
      excel('A5', '1'),
      excel('A6', '3'),
      excel('A7', 'Formula'),
      excel('A8', '=BETAINV(A2,A3,A4,A5,A6)'),
      excel('B1', 'Description'),
      excel('B2', 'Probability associated with the beta distribution'),
      excel('B3', 'Parameter of the distribution'),
      excel('B4', 'Parameter of the distribution'),
      excel('B5', 'Lower bound'),
      excel('B6', 'Upper bound'),
      excel('B7', 'Description'),
      excel('B8', 'Inverse of the cumulative beta probability density function for the parameters above'),
      excel('C7', 'Result'),

      shouldBe('A8', valueI(2)),

      exec(done)
    ]);
  });
  it('BINOMDIST', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A2', '6'),
      excel('A3', '10'),
      excel('A4', '0.5'),
      excel('A5', 'Formula'),
      excel('A6', '=BINOMDIST(A2,A3,A4,FALSE)'),
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
  it('CHIDIST', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A2', '18.307'),
      excel('A3', '10'),
      excel('A4', 'Formula'),
      excel('A5', '=CHIDIST(A2,A3)'),
      excel('B1', 'Description'),
      excel('B2', 'Value at which you want to evaluate the distribution'),
      excel('B3', 'Degrees of freedom'),
      excel('B4', 'Description'),
      excel('B5', 'One-tailed probability of the chi-squared distribution, for the arguments specified in A2 and A3.'),
      excel('C4', 'Result'),

      shouldBe('A5', valueD(0.0500006)),

      exec(done)
    ]);
  });
  it('CHIINV', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A2', '0.050001'),
      excel('A3', '10'),
      excel('A4', 'Formula'),
      excel('A5', '=CHIINV(A2,A3)'),
      excel('B1', 'Description'),
      excel('B2', 'Probability associated with the chi-squared distribution'),
      excel('B3', 'Degrees of freedom'),
      excel('B4', 'Description'),
      excel('B5', 'Inverse of the one-tailed probability of the chi-squared distribution.'),
      excel('C4', 'Result'),

      shouldBe('A5', valueD(18.306973)),

      exec(done)
    ]);
  });
  it('CHITEST', (done) => {
    _do([
      excel('A1', 'Men (Actual)'),
      excel('A10', '=CHITEST(A2:B4,A6:B8)'),
      excel('A2', '58'),
      excel('A3', '11'),
      excel('A4', '10'),
      excel('A5', 'Men (Expected)'),
      excel('A6', '45.35'),
      excel('A7', '17.56'),
      excel('A8', '16.09'),
      excel('A9', 'Formula'),
      excel('B1', 'Women (Actual)'),
      excel('B10', 'The 2 statistic for the data above is 16.16957 with 2 degrees of freedom.'),
      excel('B2', '35'),
      excel('B3', '25'),
      excel('B4', '23'),
      excel('B5', 'Women (Expected)'),
      excel('B6', '47.65'),
      excel('B7', '18.44'),
      excel('B8', '16.91'),
      excel('B9', 'Description'),
      excel('C1', 'Description'),
      excel('C2', 'Agree'),
      excel('C3', 'Neutral'),
      excel('C4', 'Disagree'),
      excel('C5', 'Description'),
      excel('C6', 'Agree'),
      excel('C7', 'Neutral'),
      excel('C8', 'Disagree'),
      excel('C9', 'Result'),

      shouldBe('A10', valueD(0.0003082)),

      exec(done)
    ]);
  });
  it('CONFIDENCE', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A2', '0.05'),
      excel('A3', '2.5'),
      excel('A4', '50'),
      excel('A5', 'Formula'),
      excel('A6', '=CONFIDENCE(A2,A3,A4)'),
      excel('B1', 'Description'),
      excel('B2', 'Significance level'),
      excel('B3', 'Standard deviation of the population'),
      excel('B4', 'Sample size'),
      excel('B5', 'Description'),
      excel('B6', 'Confidence interval for a population mean. In other words, the confidence interval for the underlying population mean for travel to work equals 30  0.692952 minutes, or 29.3 to 30.7 minutes.'),
      excel('C5', 'Result'),

      shouldBe('A6', valueD(0.692951912)),

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
  it('CRITBINOM', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A2', '6'),
      excel('A3', '0.5'),
      excel('A4', '0.75'),
      excel('A5', 'Formula'),
      excel('A6', '=CRITBINOM(A2,A3,A4)'),
      excel('B1', 'Description'),
      excel('B2', 'Number of Bernoulli trials'),
      excel('B3', 'Probability of a success on each trial'),
      excel('B4', 'Criterion value'),
      excel('B5', 'Description'),
      excel('B6', 'Smallest value for which the cumulative binomial distribution is greater than or equal to a criterion value'),
      excel('C5', 'Result'),

      shouldBe('A6', valueI(4)),

      exec(done)
    ]);
  });
  it('EXPONDIST', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A2', '0.2'),
      excel('A3', '10'),
      excel('A4', 'Formula'),
      excel('A5', '=EXPONDIST(A2,A3,TRUE)'),
      excel('A6', '=EXPONDIST(0.2,10,FALSE)'),
      excel('B1', 'Description'),
      excel('B2', 'Value of the function'),
      excel('B3', 'Parameter value'),
      excel('B4', 'Description'),
      excel('B5', 'Cumulative exponential distribution function'),
      excel('B6', 'Probability exponential distribution function'),
      excel('C4', 'Result'),

      shouldBe('A5', valueD(0.86466472)),
      shouldBe('A6', valueD(1.35335283)),

      exec(done)
    ]);
  });
  it('FDIST', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A2', '15.20686486'),
      excel('A3', '6'),
      excel('A4', '4'),
      excel('A5', 'Formula'),
      excel('A6', '=FDIST(A2,A3,A4)'),
      excel('B1', 'Description'),
      excel('B2', 'Value at which to evaluate the function.'),
      excel('B3', 'Numerator degrees of freedom.'),
      excel('B4', 'Denominator degrees of freedom'),
      excel('B5', 'Description'),
      excel('B6', 'F probability distribution for the terms in A2, A3, and A4.'),
      excel('C5', 'Result'),

      shouldBe('A6', valueD(0.01)),

      exec(done)
    ]);
  });
  it('FINV', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A2', '0.01'),
      excel('A3', '6'),
      excel('A4', '4'),
      excel('A5', 'Formula'),
      excel('A6', '=FINV(A2,A3,A4)'),
      excel('B1', 'Description'),
      excel('B2', 'Probability associated with the F cumulative distribution'),
      excel('B3', 'Numerator degrees of freedom'),
      excel('B4', 'Denominator degrees of freedom'),
      excel('B5', 'Description'),
      excel('B6', 'Inverse of the F probability distribution for the terms above'),
      excel('C5', 'Result'),

      shouldBe('A6', valueD(15.206865)),

      exec(done)
    ]);
  });
  it('FLOOR', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=FLOOR(3.7,2)'),
      excel('A3', '=FLOOR(-2.5,-2)'),
      excel('A4', '=FLOOR(2.5,-2)'),
      excel('A5', '=FLOOR(1.58,0.1)'),
      excel('A6', '=FLOOR(0.234,0.01)'),
      excel('B1', 'Description'),
      excel('B2', 'Rounds 3.7 down to nearest multiple of 2.'),
      excel('B3', 'Rounds -2.5 down to nearest multiple of -2.'),
      excel('B4', 'Returns an error value, because 2.5 and -2 have different signs.'),
      excel('B5', 'Rounds 1.58 down to the nearest multiple of 0.1.'),
      excel('B6', 'Rounds 0.234 down to the nearest multiple of 0.01.'),
      excel('C1', 'Result'),

      shouldBe('A2', valueI(2)),
      shouldBe('A3', valueI(-2)),
      shouldBe('A4', valueS('#NUM!')),
      shouldBe('A5', valueD(1.5)),
      shouldBe('A6', valueD(0.23)),

      exec(done)
    ]);
  });
  it('FTEST', (done) => {
    _do([
      excel('A1', 'Data1'),
      excel('A2', '6'),
      excel('A3', '7'),
      excel('A4', '9'),
      excel('A5', '15'),
      excel('A6', '21'),
      excel('A7', 'Formula'),
      excel('A8', '=FTEST(A2:A6,B2:B6)'),
      excel('B1', 'Data2'),
      excel('B2', '20'),
      excel('B3', '28'),
      excel('B4', '31'),
      excel('B5', '38'),
      excel('B6', '40'),
      excel('B7', 'Description'),
      excel('B8', 'F-test for the data sets above'),
      excel('C7', 'Result'),

      shouldBe('A8', valueD(0.64831785)),

      exec(done)
    ]);
  });
  it('GAMMADIST', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A2', '10.00001131'),
      excel('A3', '9'),
      excel('A4', '2'),
      excel('A5', 'Formula'),
      excel('A6', '=GAMMADIST(A2,A3,A4,FALSE)'),
      excel('A7', '=GAMMADIST(A2,A3,A4,TRUE)'),
      excel('B1', 'Description'),
      excel('B2', 'Value at which you want to evaluate the distribution'),
      excel('B3', 'Alpha parameter to the distribution'),
      excel('B4', 'Beta parameter to the distribution'),
      excel('B5', 'Description'),
      excel('B6', 'Probability density using the x, alpha, and beta values in A2, A3, A4, with FALSE cumulative argument.'),
      excel('B7', 'Cumulative distributuion  using the x, alpha, and beta values in A2, A3, A4, with TRUE cumulative argument.'),
      excel('C5', 'Result'),

      shouldBe('A6', valueD(0.032639)),
      shouldBe('A7', valueD(0.068094)),

      exec(done)
    ]);
  });
  it('GAMMAINV', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A2', '0.068094'),
      excel('A3', '9'),
      excel('A4', '2'),
      excel('A5', 'Formula'),
      excel('A6', '=GAMMAINV(A2,A3,A4)'),
      excel('B1', 'Description'),
      excel('B2', 'Probability associated with the gamma distribution'),
      excel('B3', 'Alpha parameter to the distribution'),
      excel('B4', 'Beta parameter to the distribution'),
      excel('B5', 'Description'),
      excel('B6', 'Inverse of the gamma cumulative distribution for the probability, alpha, and beta arguments in A2, A3, and A4.'),
      excel('C5', 'Result'),

      shouldBe('A6', valueD(10.0000112)),

      exec(done)
    ]);
  });
  it('HYPGEOMDIST', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A2', '1'),
      excel('A3', '4'),
      excel('A4', '8'),
      excel('A5', '20'),
      excel('A6', 'Formula'),
      excel('A7', '=HYPGEOMDIST(A2,A3,A4,A5)'),
      excel('B1', 'Description'),
      excel('B2', 'Number of successes in the sample'),
      excel('B3', 'Sample size'),
      excel('B4', 'Number of successes in the population'),
      excel('B5', 'Population size'),
      excel('B6', 'Description'),
      excel('B7', 'Hypergeometric distribution for sample and population in cells A2, A3, A4, and A5.'),
      excel('C6', 'Result'),

      shouldBe('A7', valueD(0.3633)),

      exec(done)
    ]);
  });
  it('LOGINV', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A2', '0.039084'),
      excel('A3', '3.5'),
      excel('A4', '1.2'),
      excel('A5', 'Formula'),
      excel('A6', '=LOGINV(A2, A3, A4)'),
      excel('B1', 'Description'),
      excel('B2', 'Probability associated with the lognormal distribution'),
      excel('B3', 'Mean of ln(x)'),
      excel('B4', 'Standard deviation of ln(x)'),
      excel('B5', 'Description'),
      excel('B6', 'Inverse of the lognormal cumulative distribution function, using the arguments in A2, A3, and A4.'),
      excel('C5', 'Result'),

      shouldBe('A6', valueD(4.0000252)),

      exec(done)
    ]);
  });
  it('LOGNORMDIST', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A2', '4'),
      excel('A3', '3.5'),
      excel('A4', '1.2'),
      excel('A5', 'Formula'),
      excel('A6', '=LOGNORMDIST(A2,A3,A4)'),
      excel('B1', 'Description'),
      excel('B2', 'Value at which to evaluate the function (x)'),
      excel('B3', 'Mean of ln(x)'),
      excel('B4', 'Standard deviation of ln(x)'),
      excel('B5', 'Description'),
      excel('B6', 'Cumulative lognormal distribution at 4 with the terms above'),
      excel('C5', 'Result'),

      shouldBe('A6', valueD(0.0390836)),

      exec(done)
    ]);
  });
  it('MODE', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A2', '5.6'),
      excel('A3', '4'),
      excel('A4', '4'),
      excel('A5', '3'),
      excel('A6', '2'),
      excel('A7', '4'),
      excel('A8', 'Formula'),
      excel('A9', '=MODE(A2:A7)'),
      excel('B8', 'Description'),
      excel('B9', 'Mode, or most frequently occurring number above'),
      excel('C8', 'Result'),

      shouldBe('A9', valueI(4)),

      exec(done)
    ]);
  });
  it('NEGBINOMDIST', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A2', '10'),
      excel('A3', '5'),
      excel('A4', '0.25'),
      excel('A5', 'Formula'),
      excel('A6', '=NEGBINOMDIST(A2,A3,A4)'),
      excel('B1', 'Description'),
      excel('B2', 'Number of failures'),
      excel('B3', 'Threshold number of successes'),
      excel('B4', 'Probability of a success'),
      excel('B5', 'Description'),
      excel('B6', 'Negative binomial distribution for the terms above.'),
      excel('C5', 'Result'),

      shouldBe('A6', valueD(0.05504866)),

      exec(done)
    ]);
  });
  it('NORMDIST', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A2', '42'),
      excel('A3', '40'),
      excel('A4', '1.5'),
      excel('A5', 'Formula'),
      excel('A6', '=NORMDIST(A2,A3,A4,TRUE)'),
      excel('A7', '=NORMDIST(A2,A3,A4,FALSE)'),
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
  it('NORMINV', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A2', '0.908789'),
      excel('A3', '40'),
      excel('A4', '1.5'),
      excel('A5', 'Formula'),
      excel('A6', '=NORMINV(A2,A3,A4)'),
      excel('B1', 'Description'),
      excel('B2', 'Probability corresponding to the normal distribution'),
      excel('B3', 'Arithmetic mean of the distribution'),
      excel('B4', 'Standard deviation of the distribution'),
      excel('B5', 'Description'),
      excel('B6', 'Inverse of the normal cumulative distribution for the terms above'),
      excel('C5', 'Result'),

      shouldBe('A6', valueD(42.000002)),

      exec(done)
    ]);
  });
  it('NORMSDIST', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=NORMSDIST(1.333333)'),
      excel('B1', 'Description'),
      excel('B2', 'Normal cumulative distribution function at 1.333333'),
      excel('C1', 'Result'),

      shouldBe('A2', valueD(0.908788726)),

      exec(done)
    ]);
  });
  it('NORMSINV', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=NORMSINV(0.9088)'),
      excel('B1', 'Description'),
      excel('B2', 'Inverse of the standard normal cumulative distribution, with a probability of 0.9088.'),
      excel('C1', 'Result'),

      shouldBe('A2', valueD(1.3334)),

      exec(done)
    ]);
  });
  it('PERCENTILE', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A2', '1'),
      excel('A3', '3'),
      excel('A4', '2'),
      excel('A5', '4'),
      excel('A6', 'Formula'),
      excel('A7', '=PERCENTILE(A2:A5,0.3)'),
      excel('B6', 'Description'),
      excel('B7', '30th percentile of the list in the range A2:A5.'),
      excel('C6', 'Result'),

      shouldBe('A7', valueD(1.9)),

      exec(done)
    ]);
  });
  it('PERCENTRANK', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A10', '1'),
      excel('A11', '1'),
      excel('A12', 'Formula'),
      excel('A13', '=PERCENTRANK(A2:A11,2)'),
      excel('A14', '=PERCENTRANK(A2:A11,4)'),
      excel('A15', '=PERCENTRANK(A2:A11,8)'),
      excel('A16', '=PERCENTRANK(A2:A11,5)'),
      excel('A2', '13'),
      excel('A3', '12'),
      excel('A4', '11'),
      excel('A5', '8'),
      excel('A6', '4'),
      excel('A7', '3'),
      excel('A8', '2'),
      excel('A9', '1'),
      excel('B12', 'Description (Result)'),
      excel('B13', 'Percent rank of 2 in the range A2:A11. Result is 0.333 because three values in the set are less than 2, and six are greater than 2. Because 2 is found in the range (cell A8), the number of values less than 2 is divided by the sum of the number of values less than 2 and the number of values greater than 2. That is, 3/(3+6)=0.333.'),
      excel('B14', 'Percent rank of 4 in the range A2:A11. Five values are less than 4, and four are greater. Following the example above, 5/(4+5)=0.555.'),
      excel('B15', 'Percent rank of 8 in the range A2:A11. Six values are less than 8, and 3 are greater. Following the example above,  6/(6+3) =0.666.'),
      excel('B16', 'Percent rank of 5 in the range A2:A11. Unlike the examples above, 5 is not found in the range. The PERCENTRANK of 5 is calculated by finding the one-quarter mark between the PERCENTRANK of 4 and the PERCENTRANK of 8. This is (0.555)+(0.25*(0.666-0.555)), or 0.583.'),
      excel('C12', 'Result'),

      shouldBe('A13', valueD(0.333)),
      shouldBe('A14', valueD(0.555)),
      shouldBe('A15', valueD(0.666)),
      shouldBe('A16', valueD(0.583)),

      exec(done)
    ]);
  });
  it('POISSON', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A2', '2'),
      excel('A3', '5'),
      excel('A4', 'Formula'),
      excel('A5', '=POISSON(A2,A3,TRUE)'),
      excel('A6', '=POISSON(A2,A3,FALSE)'),
      excel('B1', 'Description'),
      excel('B2', 'Number of events'),
      excel('B3', 'Expected mean'),
      excel('B4', 'Description (Result)'),
      excel('B5', 'Cumulative Poisson probability with the terms above (0.124652)'),
      excel('B6', 'Poisson probability mass function with the terms above (0.084224)'),
      excel('C4', 'Result'),

      shouldBe('A5', valueD(0.124652)),
      shouldBe('A6', valueD(0.084224)),

      exec(done)
    ]);
  });
  it('QUARTILE', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A10', 'Formula'),
      excel('A11', '=QUARTILE(A2:A9,1)'),
      excel('A2', '1'),
      excel('A3', '2'),
      excel('A4', '4'),
      excel('A5', '7'),
      excel('A6', '8'),
      excel('A7', '9'),
      excel('A8', '10'),
      excel('A9', '12'),
      excel('B10', 'Description (Result)'),
      excel('B11', 'First quartile (25th percentile) of the data above (3.5)'),
      excel('C10', 'Result'),

      shouldBe('A11', valueD(3.5)),

      exec(done)
    ]);
  });
  it('RANK', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A2', '7'),
      excel('A3', '3.5'),
      excel('A4', '3.5'),
      excel('A5', '1'),
      excel('A6', '2'),
      excel('A7', 'Formula'),
      excel('A8', '=RANK(A3,A2:A6,1)'),
      excel('A9', '=RANK(A2,A2:A6,1)'),
      excel('B7', 'Description (Result)'),
      excel('B8', 'Rank of 3.5 in the list above (3)'),
      excel('B9', 'Rank of 7 in the list above (5)'),
      excel('C7', 'Result'),

      shouldBe('A8', valueI(3)),
      shouldBe('A9', valueI(5)),

      exec(done)
    ]);
  });
  it('STDEV', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A10', '1350'),
      excel('A11', '1303'),
      excel('A12', '1299'),
      excel('A13', 'Formula'),
      excel('A14', '=STDEV(A3:A12)'),
      excel('A2', 'Strength'),
      excel('A3', '1345'),
      excel('A4', '1301'),
      excel('A5', '1368'),
      excel('A6', '1322'),
      excel('A7', '1310'),
      excel('A8', '1370'),
      excel('A9', '1318'),
      excel('B13', 'Description (Result)'),
      excel('B14', 'Standard deviation of breaking strength (27.46392)'),
      excel('C13', 'Result'),

      shouldBe('A14', valueD(27.46392)),

      exec(done)
    ]);
  });
  it('STDEVP', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A10', '1350'),
      excel('A11', '1303'),
      excel('A12', '1299'),
      excel('A13', 'Formula'),
      excel('A14', '=STDEVP(A3:A12)'),
      excel('A2', 'Strength'),
      excel('A3', '1345'),
      excel('A4', '1301'),
      excel('A5', '1368'),
      excel('A6', '1322'),
      excel('A7', '1310'),
      excel('A8', '1370'),
      excel('A9', '1318'),
      excel('B13', 'Description (Result)'),
      excel('B14', 'Standard deviation of breaking strength, assuming only 10 tools are produced (26.0546).'),
      excel('C13', 'Result'),

      shouldBe('A14', valueD(26.05456)),

      exec(done)
    ]);
  });
  it('TDIST', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A2', '1.959999998'),
      excel('A3', '60'),
      excel('A4', 'Formula'),
      excel('A5', '=TDIST(A2,A3,2)'),
      excel('A6', '=TDIST(A2,A3,1)'),
      excel('B1', 'Description'),
      excel('B2', 'Value at which to evaluate the distribution'),
      excel('B3', 'Degrees of freedom'),
      excel('B4', 'Description (Result)'),
      excel('B5', 'Two-tailed distribution (0.054644930, or 5.46 percent)'),
      excel('B6', 'One-tailed distribution (0.027322465 or 2.73 percent)'),
      excel('C4', 'Result'),

      shouldBe('A5', valueS('5.46%')),
      shouldBe('A6', valueS('2.73%')),

      exec(done)
    ]);
  });
  it('TINV', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A2', '0.05464'),
      excel('A3', '60'),
      excel('A4', 'Formula'),
      excel('A5', '=TINV(A2,A3)'),
      excel('B1', 'Description'),
      excel('B2', 'Probability associated with the Student\'s two-tailed t-distribution'),
      excel('B3', 'Degrees of freedom'),
      excel('B4', 'Description'),
      excel('B5', 'The t-value of the Student\'s  t-distribution based on the arguments in A2 and A3.'),
      excel('C4', 'Result'),

      shouldBe('A5', valueD(1.96)),

      exec(done)
    ]);
  });
  it('TTEST', (done) => {
    _do([
      excel('A1', 'Data 1'),
      excel('A10', '5'),
      excel('A11', 'Formula'),
      excel('A12', '=TTEST(A2:A10,B2:B10,2,1)'),
      excel('A2', '3'),
      excel('A3', '4'),
      excel('A4', '5'),
      excel('A5', '8'),
      excel('A6', '9'),
      excel('A7', '1'),
      excel('A8', '2'),
      excel('A9', '4'),
      excel('B1', 'Data 2'),
      excel('B10', '1'),
      excel('B11', 'Description (Result)'),
      excel('B12', 'Probability associated with a Student\'s paired t-Test, with a two-tailed distribution. Uses values in A2:A10 and B2:B10.'),
      excel('B2', '6'),
      excel('B3', '19'),
      excel('B4', '3'),
      excel('B5', '2'),
      excel('B6', '14'),
      excel('B7', '4'),
      excel('B8', '5'),
      excel('B9', '17'),
      excel('C11', 'Result'),

      shouldBe('A12', valueD(0.19602)),

      exec(done)
    ]);
  });
  it('VAR', (done) => {
    _do([
      excel('A1', 'Strength'),
      excel('A10', '1303'),
      excel('A11', '1299'),
      excel('A12', 'Formula'),
      excel('A13', '=VAR(A2:A11)'),
      excel('A2', '1345'),
      excel('A3', '1301'),
      excel('A4', '1368'),
      excel('A5', '1322'),
      excel('A6', '1310'),
      excel('A7', '1370'),
      excel('A8', '1318'),
      excel('A9', '1350'),
      excel('B12', 'Description'),
      excel('B13', 'Variance for the breaking strength of the tools tested.'),
      excel('C12', 'Result'),

      shouldBe('A13', valueD(754.2667)),

      exec(done)
    ]);
  });
  it('VARP', (done) => {
    _do([
      excel('A1', 'Strength'),
      excel('A10', '1303'),
      excel('A11', '1299'),
      excel('A12', 'Formula'),
      excel('A13', '=VARP(A2:A11)'),
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
      excel('B14', 'This example uses the VAR function, which assumes only a sample of the population, and returns a different result.'),
      excel('C12', 'Result'),

      shouldBe('A13', valueD(678.84)),
      shouldBe('A14', valueD(754.27)),

      exec(done)
    ]);
  });
  it('WEIBULL', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A2', '105'),
      excel('A3', '20'),
      excel('A4', '100'),
      excel('A5', 'Formula'),
      excel('A6', '=WEIBULL(A2,A3,A4,TRUE)'),
      excel('A7', '=WEIBULL(A2,A3,A4,FALSE)'),
      excel('B1', 'Description'),
      excel('B2', 'Value at which to evaluate the function'),
      excel('B3', 'Alpha parameter to the distribution'),
      excel('B4', 'Beta parameter to the distribution'),
      excel('B5', 'Description (Result)'),
      excel('B6', 'Weibull cumulative distribution function for the terms above (0.929581)'),
      excel('B7', 'Weibull probability density function for the terms above (0.035589)'),
      excel('C5', 'Result'),

      shouldBe('A6', valueD(0.929581)),
      shouldBe('A7', valueD(0.035589)),

      exec(done)
    ]);
  });
  it('ZTEST', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A10', '1'),
      excel('A11', '9'),
      excel('A12', 'Formula'),
      excel('A13', '=ZTEST(A2:A11,4)'),
      excel('A14', '=2 * MIN(ZTEST(A2:A11,4), 1 - ZTEST(A2:A11,4))'),
      excel('A15', '=ZTEST(A2:A11,6)'),
      excel('A16', '=2 * MIN(ZTEST(A2:A11,6), 1 - ZTEST(A2:A11,6))'),
      excel('A2', '3'),
      excel('A3', '6'),
      excel('A4', '7'),
      excel('A5', '8'),
      excel('A6', '6'),
      excel('A7', '5'),
      excel('A8', '4'),
      excel('A9', '2'),
      excel('B12', 'Description (Result)'),
      excel('B13', 'One-tailed probability-value of a z-test for the data set above, at the hypothesized population mean of 4 (0.090574)'),
      excel('B14', 'Two-tailed probability-value of a z-test for the data set above, at the hypothesized population mean of 4 (0.181148)'),
      excel('B15', 'One-tailed probability-value of a z-test for the data set above, at the hypothesized population mean of 6 (0.863043)'),
      excel('B16', 'Two-tailed probability-value of a z-test for the data set above, at the hypothesized population mean of 6 (0.273913)'),
      excel('C12', 'Result'),

      shouldBe('A13', valueD(0.090574)),
      shouldBe('A14', valueD(0.181148)),
      shouldBe('A15', valueD(0.863043)),
      shouldBe('A16', valueD(0.273913)),

      exec(done)
    ]);
  });
  it('DAVERAGE', (done) => {
    _do([
      excel('A1', 'Tree'),
      excel('A10', 'Apple'),
      excel('A11', 'Formula'),
      excel('A12', '=DAVERAGE(A4:E10, "Yield", A1:B2)'),
      excel('A13', '=DAVERAGE(A4:E10, 3, A4:E10)'),
      excel('A2', '=Apple'),
      excel('A3', '=Pear'),
      excel('A4', 'Tree'),
      excel('A5', 'Apple'),
      excel('A6', 'Pear'),
      excel('A7', 'Cherry'),
      excel('A8', 'Apple'),
      excel('A9', 'Pear'),
      excel('B1', 'Height'),
      excel('B10', '8'),
      excel('B11', 'Description'),
      excel('B12', 'The average yield of apple trees over 10 feet in height.'),
      excel('B13', 'The average age of all trees in the database.'),
      excel('B2', '>10'),
      excel('B4', 'Height'),
      excel('B5', '18'),
      excel('B6', '12'),
      excel('B7', '13'),
      excel('B8', '14'),
      excel('B9', '9'),
      excel('C1', 'Age'),
      excel('C10', '9'),
      excel('C11', 'Result'),
      excel('C4', 'Age'),
      excel('C5', '20'),
      excel('C6', '12'),
      excel('C7', '14'),
      excel('C8', '15'),
      excel('C9', '8'),
      excel('D1', 'Yield'),
      excel('D10', '6'),
      excel('D4', 'Yield'),
      excel('D5', '14'),
      excel('D6', '10'),
      excel('D7', '9'),
      excel('D8', '10'),
      excel('D9', '8'),
      excel('E1', 'Profit'),
      excel('E10', '45'),
      excel('E4', 'Profit'),
      excel('E5', '105'),
      excel('E6', '96'),
      excel('E7', '105'),
      excel('E8', '75'),
      excel('E9', '76.8'),
      excel('F1', 'Height'),
      excel('F2', '<16'),

      shouldBe('A12', valueI(12)),
      shouldBe('A13', valueI(13)),

      exec(done)
    ]);
  });
  it('DCOUNT', (done) => {
    _do([
      excel('A1', 'Tree'),
      excel('A10', 'Pear'),
      excel('A11', 'Apple'),
      excel('A12', 'Formula'),
      excel('A13', '=DCOUNT(A5:E11, "Age", A1:F2)'),
      excel('A2', '="=Apple"'),
      excel('A3', '="=Pear"'),
      excel('A5', 'Tree'),
      excel('A6', 'Apple'),
      excel('A7', 'Pear'),
      excel('A8', 'Cherry'),
      excel('A9', 'Apple'),
      excel('B1', 'Height'),
      excel('B10', '9'),
      excel('B11', '12'),
      excel('B12', 'Description'),
      excel('B13', 'Finds apple trees between a height of 10 and 16 and counts how many of the Age fields in those records contain numbers.'),
      excel('B2', '>10'),
      excel('B5', 'Height'),
      excel('B6', '18'),
      excel('B7', '12'),
      excel('B8', '13'),
      excel('B9', '14'),
      excel('C1', 'Age'),
      excel('C10', '8'),
      excel('C11', '11'),
      excel('C12', 'Result'),
      excel('C5', 'Age'),
      excel('C6', '20'),
      excel('C7', '12'),
      excel('C8', '14'),
      excel('C9', 'N/A'),
      excel('D1', 'Yield'),
      excel('D10', '8'),
      excel('D11', '6'),
      excel('D5', 'Yield'),
      excel('D6', '14'),
      excel('D7', '10'),
      excel('D8', '9'),
      excel('D9', '10'),
      excel('E1', 'Profit'),
      excel('E10', '$77'),
      excel('E11', '$45'),
      excel('E5', 'Profit'),
      excel('E6', '$105'),
      excel('E7', '$96'),
      excel('E8', '$105'),
      excel('E9', '$75'),
      excel('F1', 'Height'),
      excel('F2', '<16'),

      shouldBe('A13', valueI(1)),

      exec(done)
    ]);
  });
  it('DCOUNTA', (done) => {
    _do([
      excel('A1', 'Tree'),
      excel('A10', 'Apple'),
      excel('A11', 'Formula'),
      excel('A12', '=DCOUNTA(A4:E10, "Profit", A1:F2)'),
      excel('A2', '="=Apple"'),
      excel('A3', '="=Pear"'),
      excel('A4', 'Tree'),
      excel('A5', 'Apple'),
      excel('A6', 'Pear'),
      excel('A7', 'Cherry'),
      excel('A8', 'Apple'),
      excel('A9', 'Pear'),
      excel('B1', 'Height'),
      excel('B10', '8'),
      excel('B11', 'Description'),
      excel('B12', 'Counts the rows (1) containing "Apple" in column A with a height >10 and <16. Only row 8 satisfies these three conditions.'),
      excel('B2', '>10'),
      excel('B4', 'Height'),
      excel('B5', '18'),
      excel('B6', '12'),
      excel('B7', '13'),
      excel('B8', '14'),
      excel('B9', '9'),
      excel('C1', 'Age'),
      excel('C10', '9'),
      excel('C11', 'Result'),
      excel('C4', 'Age'),
      excel('C5', '20'),
      excel('C6', '12'),
      excel('C7', '14'),
      excel('C8', '15'),
      excel('C9', '8'),
      excel('D1', 'Yield'),
      excel('D10', '6'),
      excel('D4', 'Yield'),
      excel('D5', '14'),
      excel('D6', '10'),
      excel('D7', '9'),
      excel('D8', '10'),
      excel('D9', '8'),
      excel('E1', 'Profit'),
      excel('E10', '45.0'),
      excel('E4', 'Profit'),
      excel('E5', '105.0'),
      excel('E6', '96.0'),
      excel('E7', '105.0'),
      excel('E8', '75.0'),
      excel('E9', '76.8'),
      excel('F1', 'Height'),
      excel('F2', '<16'),

      shouldBe('A12', valueI(1)),

      exec(done)
    ]);
  });
  it('DCOUNTA', (done) => {
    _do([
      excel('A10', 'Produce'),
      excel('A12', 'Formula'),
      excel('A13', '\'=DCOUNTA(A6:C10,2,B1:B3)'),
      excel('A6', 'Category'),
      excel('A7', 'Beverages'),
      excel('A8', 'Meat'),
      excel('A9', 'produce'),
      excel('B1', 'Salesperson'),
      excel('B10', 'Davolio'),
      excel('B12', 'Description'),
      excel('B13', 'Counts the number of rows (3) in A6:C10 that meet either of the "Salesperson" conditions in rows 2 and 3.'),
      excel('B2', '="=Davolio"'),
      excel('B3', '="=Buchanan"'),
      excel('B6', 'Salesperson'),
      excel('B7', 'Suyama'),
      excel('B8', 'Davolio'),
      excel('B9', 'Buchanan'),
      excel('C10', '$6,544'),
      excel('C12', 'Result'),
      excel('C6', 'Sales'),
      excel('C7', '$5,122'),
      excel('C8', '$450'),
      excel('C9', '$6,328'),

      shouldBe('A13', valueS('=DCOUNTA(A6:C10,2,B1:B3)')),

      exec(done)
    ]);
  });
  it('DCOUNTA', (done) => {
    _do([
      excel('A1', 'Category'),
      excel('A10', 'Produce'),
      excel('A11', 'Beverages'),
      excel('A12', 'Produce'),
      excel('A14', 'Formula'),
      excel('A15', '\'=DCOUNTA(A6:C12,,A1:C2)'),
      excel('A2', '="=Produce"'),
      excel('A6', 'Category'),
      excel('A7', 'Beverages'),
      excel('A8', 'Meat'),
      excel('A9', 'Produce'),
      excel('B1', 'Salesperson'),
      excel('B10', 'Davolio'),
      excel('B11', 'Buchanan'),
      excel('B12', 'Davolio'),
      excel('B14', 'Description'),
      excel('B15', 'Counts the number of rows (2) in A6:C12 that meet the conditions in row 2 (="Produce" and >2000).'),
      excel('B6', 'Salesperson'),
      excel('B7', 'Suyama'),
      excel('B8', 'Davolio'),
      excel('B9', 'Buchanan'),
      excel('C1', 'Sales'),
      excel('C10', '$6,544'),
      excel('C11', '$3,677'),
      excel('C12', '$3,186'),
      excel('C14', 'Result'),
      excel('C6', 'Sales'),
      excel('C7', '$5,122'),
      excel('C8', '$450'),
      excel('C9', '$935'),

      shouldBe('A15', valueS('=DCOUNTA(A6:C12,,A1:C2)')),
      shouldBe('A2', valueS('>2000')),

      exec(done)
    ]);
  });
  it('DCOUNTA', (done) => {
    _do([
      excel('A1', 'Category'),
      excel('A10', 'Produce'),
      excel('A12', 'Formula'),
      excel('A13', '\'=DCOUNTA(A6:C10,"Sales",A1:B3)'),
      excel('A2', '="=Produce"'),
      excel('A6', 'Category'),
      excel('A7', 'Beverages'),
      excel('A8', 'Meat'),
      excel('A9', 'produce'),
      excel('B1', 'Salesperson'),
      excel('B10', 'Buchanan'),
      excel('B12', 'Description'),
      excel('B13', 'Counts the number of rows (2) in A6:C10 that meet either of the conditions in A1:C3, where the "Sales" field is not empty.'),
      excel('B3', '="=Davolio"'),
      excel('B6', 'Salesperson'),
      excel('B7', 'Suyama'),
      excel('B8', 'Davolio'),
      excel('B9', 'Buchanan'),
      excel('C12', 'Result'),
      excel('C6', 'Sales'),
      excel('C7', '$5,122'),
      excel('C8', '$675'),
      excel('C9', '$937'),

      shouldBe('A13', valueS('=DCOUNTA(A6:C10,"Sales",A1:B3)')),

      exec(done)
    ]);
  });
  it('DCOUNTA', (done) => {
    _do([
      excel('A1', 'Category'),
      excel('A10', 'Produce'),
      excel('A12', 'Formula'),
      excel('A13', '\'=DCOUNTA(A6:C10,,B1:C3)'),
      excel('A6', 'Category'),
      excel('A7', 'Beverages'),
      excel('A8', 'Meat'),
      excel('A9', 'produce'),
      excel('B1', 'Salesperson'),
      excel('B10', 'Davolio'),
      excel('B12', 'Description'),
      excel('B13', 'Counts the number of rows (2) in A6:C10 that meet all conditions in B1:C3.'),
      excel('B2', '="=Davolio"'),
      excel('B3', '="=Buchanan"'),
      excel('B6', 'Salesperson'),
      excel('B7', 'Suyama'),
      excel('B8', 'Davolio'),
      excel('B9', 'Buchanan'),
      excel('C1', 'Sales'),
      excel('C10', '$6,544'),
      excel('C12', 'Result'),
      excel('C6', 'Sales'),
      excel('C7', '$5,122'),
      excel('C8', '$450'),
      excel('C9', '$6,328'),

      shouldBe('A13', valueS('=DCOUNTA(A6:C10,,B1:C3)')),
      shouldBe('A2', valueS('>3000')),
      shouldBe('A3', valueS('>1500')),

      exec(done)
    ]);
  });
  it('DCOUNTA', (done) => {
    _do([
      excel('A1', 'Category'),
      excel('A10', 'Produce'),
      excel('A12', 'Formula'),
      excel('A13', '\'=DCOUNTA(A6:C10,,C1:D3)'),
      excel('A6', 'Category'),
      excel('A7', 'Beverages'),
      excel('A8', 'Meat'),
      excel('A9', 'produce'),
      excel('B1', 'Salesperson'),
      excel('B10', 'Davolio'),
      excel('B12', 'Description'),
      excel('B13', 'Counts the number of rows (2) that meet the conditions in row 2 (>6000 and <6500) or meet the condition in row 3 (<500).'),
      excel('B6', 'Salesperson'),
      excel('B7', 'Suyama'),
      excel('B8', 'Davolio'),
      excel('B9', 'Buchanan'),
      excel('C1', 'Sales'),
      excel('C10', '$6,544'),
      excel('C12', 'Result'),
      excel('C2', '>6000'),
      excel('C3', '<500'),
      excel('C6', 'Sales'),
      excel('C7', '$5,122'),
      excel('C8', '$450'),
      excel('C9', '$6,328'),
      excel('D1', 'Sales'),
      excel('D2', '<6500'),

      shouldBe('A13', valueS('=DCOUNTA(A6:C10,,C1:D3)')),

      exec(done)
    ]);
  });
  it('DCOUNTA', (done) => {
    _do([
      excel('A1', 'Category'),
      excel('A10', 'Produce'),
      excel('A12', 'Formula'),
      excel('A13', '\'=DCOUNTA(A6:C10,,A1:B3)'),
      excel('A2', 'Me'),
      excel('A6', 'Category'),
      excel('A7', 'Beverages'),
      excel('A8', 'Meat'),
      excel('A9', 'produce'),
      excel('B1', 'Salesperson'),
      excel('B10', 'Davolio'),
      excel('B12', 'Description'),
      excel('B13', 'Counts the number of rows (3) that meet either of the conditions in A1:B3.'),
      excel('B3', '?u*'),
      excel('B6', 'Salesperson'),
      excel('B7', 'Suyama'),
      excel('B8', 'Davolio'),
      excel('B9', 'Buchanan'),
      excel('C1', 'Sales'),
      excel('C10', '$6,544'),
      excel('C12', 'Result'),
      excel('C6', 'Sales'),
      excel('C7', '$5,122'),
      excel('C8', '$450'),
      excel('C9', '$6,328'),

      shouldBe('A13', valueS('=DCOUNTA(A6:C10,,A1:B3)')),

      exec(done)
    ]);
  });
  it('DCOUNTA', (done) => {
    _do([
      excel('A10', 'Produce'),
      excel('A12', 'Formula'),
      excel('A13', '\'=DCOUNTA(A6:C10,,C1:C2)'),
      excel('A6', 'Category'),
      excel('A7', 'Beverages'),
      excel('A8', 'Meat'),
      excel('A9', 'produce'),
      excel('B10', 'Davolio'),
      excel('B12', 'Description'),
      excel('B13', 'Counts the number of rows (3) that meet the condition (>4611) in C1:C2. The condition in C2 is created by concatenating =">" with cell C4, which is the calculated average of  C7:C10.'),
      excel('B6', 'Salesperson'),
      excel('B7', 'Suyama'),
      excel('B8', 'Davolio'),
      excel('B9', 'Buchanan'),
      excel('C1', 'Sales'),
      excel('C10', '$6,544'),
      excel('C12', 'Result'),
      excel('C3', 'Calculated Average'),
      excel('C6', 'Sales'),
      excel('C7', '$5,122'),
      excel('C8', '$450'),
      excel('C9', '$6,328'),

      shouldBe('A13', valueS('=DCOUNTA(A6:C10,,C1:C2)')),
      shouldBe('A2', valueS('=CONCATENATE(">",C4)')),
      shouldBe('A4', valueS('=AVERAGE(C7:C10)')),

      exec(done)
    ]);
  });
  it('DGET', (done) => {
    _do([
      excel('A1', 'Tree'),
      excel('A10', 'Pear'),
      excel('A11', 'Apple'),
      excel('A12', 'Formula'),
      excel('A13', '=DGET(A5:E11, "Yield", A1:A3)'),
      excel('A15', '=DGET(A5:E11, "Yield", A1:F3)'),
      excel('A2', '="=Apple"'),
      excel('A3', '="Pear"'),
      excel('A5', 'Tree'),
      excel('A6', 'Apple'),
      excel('A7', 'Pear'),
      excel('A8', 'Cherry'),
      excel('A9', 'Apple'),
      excel('B1', 'Height'),
      excel('B10', '9'),
      excel('B11', '8'),
      excel('B12', 'Description'),
      excel('B13', 'Returns the #NUM! error value because more than one record meets the criteria (any apple or pear tree).'),
      excel('B15', 'Returns 10 (the yield of the apple tree in row 9) because that is the only record that meets the conditions in A1:F3.'),
      excel('B2', '>10'),
      excel('B3', '>12'),
      excel('B5', 'Height'),
      excel('B6', '18'),
      excel('B7', '12'),
      excel('B8', '13'),
      excel('B9', '14'),
      excel('C1', 'Age'),
      excel('C10', '8'),
      excel('C11', '9'),
      excel('C12', 'Result'),
      excel('C5', 'Age'),
      excel('C6', '20'),
      excel('C7', '12'),
      excel('C8', '14'),
      excel('C9', '15'),
      excel('D1', 'Yield'),
      excel('D10', '8'),
      excel('D11', '6'),
      excel('D5', 'Yield'),
      excel('D6', '14'),
      excel('D7', '10'),
      excel('D8', '9'),
      excel('D9', '10'),
      excel('E1', 'Profit'),
      excel('E10', '$77'),
      excel('E11', '$45'),
      excel('E5', 'Profit'),
      excel('E6', '$105'),
      excel('E7', '$96'),
      excel('E8', '$105'),
      excel('E9', '$75'),
      excel('F1', 'Height'),
      excel('F2', '<16'),

      shouldBe('A13', valueS('#NUM!')),
      shouldBe('A15', valueI(10)),

      exec(done)
    ]);
  });
  it('DMAX', (done) => {
    _do([
      excel('A1', 'Tree'),
      excel('A10', 'Pear'),
      excel('A11', 'Apple'),
      excel('A12', 'Formula'),
      excel('A13', '=DMAX(A5:E11, "Profit", A1:F3)'),
      excel('A2', '="=Apple"'),
      excel('A3', '="=Pear"'),
      excel('A5', 'Tree'),
      excel('A6', 'Apple'),
      excel('A7', 'Pear'),
      excel('A8', 'Cherry'),
      excel('A9', 'Apple'),
      excel('B1', 'Height'),
      excel('B10', '9'),
      excel('B11', '8'),
      excel('B12', 'Description'),
      excel('B13', 'The maximum profit of either any apple tree with a height between 10 and 16 feet or any pear tree. The pear tree in row 7 meets these conditions.'),
      excel('B2', '>10'),
      excel('B5', 'Height'),
      excel('B6', '18'),
      excel('B7', '12'),
      excel('B8', '13'),
      excel('B9', '14'),
      excel('C1', 'Age'),
      excel('C10', '8'),
      excel('C11', '9'),
      excel('C12', 'Result'),
      excel('C5', 'Age'),
      excel('C6', '20'),
      excel('C7', '12'),
      excel('C8', '14'),
      excel('C9', '15'),
      excel('D1', 'Yield'),
      excel('D10', '8'),
      excel('D11', '6'),
      excel('D5', 'Yield'),
      excel('D6', '14'),
      excel('D7', '10'),
      excel('D8', '9'),
      excel('D9', '10'),
      excel('E1', 'Profit'),
      excel('E10', '$77'),
      excel('E11', '$45'),
      excel('E5', 'Profit'),
      excel('E6', '$105'),
      excel('E7', '$96'),
      excel('E8', '$105'),
      excel('E9', '$75'),
      excel('F1', 'Height'),
      excel('F2', '<16'),

      shouldBe('A13', valueS('$96')),

      exec(done)
    ]);
  });
  it('DMIN', (done) => {
    _do([
      excel('A1', 'Tree'),
      excel('A10', 'Pear'),
      excel('A11', 'Apple'),
      excel('A12', 'Formula'),
      excel('A13', '=DMIN(A5:E11, "Profit", A1:F3)'),
      excel('A2', '="=Apple"'),
      excel('A3', '="=Pear"'),
      excel('A5', 'Tree'),
      excel('A6', 'Apple'),
      excel('A7', 'Pear'),
      excel('A8', 'Cherry'),
      excel('A9', 'Apple'),
      excel('B1', 'Height'),
      excel('B10', '9'),
      excel('B11', '8'),
      excel('B12', 'Description'),
      excel('B13', 'The minimum profit of either apple trees between 10 feet and 16 feet in height, or any pear tree. The apple tree in row 9 meets these conditions.'),
      excel('B2', '>10'),
      excel('B5', 'Height'),
      excel('B6', '18'),
      excel('B7', '12'),
      excel('B8', '13'),
      excel('B9', '14'),
      excel('C1', 'Age'),
      excel('C10', '8'),
      excel('C11', '9'),
      excel('C12', 'Result'),
      excel('C5', 'Age'),
      excel('C6', '20'),
      excel('C7', '12'),
      excel('C8', '14'),
      excel('C9', '15'),
      excel('D1', 'Yield'),
      excel('D10', '8'),
      excel('D11', '6'),
      excel('D5', 'Yield'),
      excel('D6', '14'),
      excel('D7', '10'),
      excel('D8', '9'),
      excel('D9', '10'),
      excel('E1', 'Profit'),
      excel('E10', '$77'),
      excel('E11', '$45'),
      excel('E5', 'Profit'),
      excel('E6', '$105'),
      excel('E7', '$96'),
      excel('E8', '$105'),
      excel('E9', '$75'),
      excel('F1', 'Height'),
      excel('F2', '<16'),

      shouldBe('A13', valueS('$75')),

      exec(done)
    ]);
  });
  it('DPRODUCT', (done) => {
    _do([
      excel('A1', 'Tree'),
      excel('A10', 'Pear'),
      excel('A11', 'Apple'),
      excel('A12', 'Formula'),
      excel('A13', '=DPRODUCT(A5:E11, "Yield", A1:F3)'),
      excel('A2', '=Apple'),
      excel('A3', '=Pear'),
      excel('A5', 'Tree'),
      excel('A6', 'Apple'),
      excel('A7', 'Pear'),
      excel('A8', 'Cherry'),
      excel('A9', 'Apple'),
      excel('B1', 'Height'),
      excel('B10', '9'),
      excel('B11', '8'),
      excel('B12', 'Description'),
      excel('B13', 'The product of the yields from apple trees with a height between 10 and 16 feet and any pear trees.'),
      excel('B2', '>10'),
      excel('B5', 'Height'),
      excel('B6', '18'),
      excel('B7', '12'),
      excel('B8', '13'),
      excel('B9', '14'),
      excel('C1', 'Age'),
      excel('C10', '8'),
      excel('C11', '9'),
      excel('C12', 'Result'),
      excel('C5', 'Age'),
      excel('C6', '20'),
      excel('C7', '12'),
      excel('C8', '14'),
      excel('C9', '15'),
      excel('D1', 'Yield'),
      excel('D10', '8'),
      excel('D11', '6'),
      excel('D5', 'Yield'),
      excel('D6', '14'),
      excel('D7', '10'),
      excel('D8', '9'),
      excel('D9', '10'),
      excel('E1', 'Profit'),
      excel('E10', '$77'),
      excel('E11', '$45'),
      excel('E5', 'Profit'),
      excel('E6', '$105'),
      excel('E7', '$96'),
      excel('E8', '$105'),
      excel('E9', '$75'),
      excel('F1', 'Height'),
      excel('F2', '<16'),

      shouldBe('A13', valueS('$800')),

      exec(done)
    ]);
  });
  it('DSTDEV', (done) => {
    _do([
      excel('A1', 'Tree'),
      excel('A10', 'Pear'),
      excel('A11', 'Apple'),
      excel('A12', 'Formula'),
      excel('A13', '=DSTDEV(A5:E11, "Yield", A1:A3)'),
      excel('A2', '="=Apple"'),
      excel('A3', '="=Pear"'),
      excel('A5', 'Tree'),
      excel('A6', 'Apple'),
      excel('A7', 'Pear'),
      excel('A8', 'Cherry'),
      excel('A9', 'Apple'),
      excel('B1', 'Height'),
      excel('B10', '9'),
      excel('B11', '8'),
      excel('B12', 'Description'),
      excel('B13', 'The estimated standard deviation in the yield of apple and pear trees if the data in A5:E11 is only a sample of the total orchard population.'),
      excel('B2', '>10'),
      excel('B5', 'Height'),
      excel('B6', '18'),
      excel('B7', '12'),
      excel('B8', '13'),
      excel('B9', '14'),
      excel('C1', 'Age'),
      excel('C10', '8'),
      excel('C11', '9'),
      excel('C12', 'Result'),
      excel('C5', 'Age'),
      excel('C6', '20'),
      excel('C7', '12'),
      excel('C8', '14'),
      excel('C9', '15'),
      excel('D1', 'Yield'),
      excel('D10', '8'),
      excel('D11', '6'),
      excel('D5', 'Yield'),
      excel('D6', '14'),
      excel('D7', '10'),
      excel('D8', '9'),
      excel('D9', '10'),
      excel('E1', 'Profit'),
      excel('E10', '$77'),
      excel('E11', '$45'),
      excel('E5', 'Profit'),
      excel('E6', '$105'),
      excel('E7', '$96'),
      excel('E8', '$105'),
      excel('E9', '$75'),
      excel('F1', 'Height'),
      excel('F2', '<16'),

      shouldBe('A13', valueD(2.96648)),

      exec(done)
    ]);
  });
  it('DSTDEVP', (done) => {
    _do([
      excel('A1', 'Tree'),
      excel('A10', 'Pear'),
      excel('A11', 'Apple'),
      excel('A12', 'Formula'),
      excel('A13', '=DSTDEVP(A5:E11, "Yield", A1:A3)'),
      excel('A2', '="=Apple"'),
      excel('A3', '="=Pear"'),
      excel('A5', 'Tree'),
      excel('A6', 'Apple'),
      excel('A7', 'Pear'),
      excel('A8', 'Cherry'),
      excel('A9', 'Apple'),
      excel('B1', 'Height'),
      excel('B10', '9'),
      excel('B11', '8'),
      excel('B12', 'Description'),
      excel('B13', 'The true standard deviation in the yield of apple and pear trees if the data in the database is the entire population.'),
      excel('B2', '>10'),
      excel('B5', 'Height'),
      excel('B6', '18'),
      excel('B7', '12'),
      excel('B8', '13'),
      excel('B9', '14'),
      excel('C1', 'Age'),
      excel('C10', '8'),
      excel('C11', '9'),
      excel('C12', 'Result'),
      excel('C5', 'Age'),
      excel('C6', '20'),
      excel('C7', '12'),
      excel('C8', '14'),
      excel('C9', '15'),
      excel('D1', 'Yield'),
      excel('D10', '8'),
      excel('D11', '6'),
      excel('D5', 'Yield'),
      excel('D6', '14'),
      excel('D7', '10'),
      excel('D8', '9'),
      excel('D9', '10'),
      excel('E1', 'Profit'),
      excel('E10', '$77'),
      excel('E11', '$45'),
      excel('E5', 'Profit'),
      excel('E6', '$105'),
      excel('E7', '$96'),
      excel('E8', '$105'),
      excel('E9', '$75'),
      excel('F1', 'Height'),
      excel('F2', '<16'),

      shouldBe('A13', valueD(2.6532998)),

      exec(done)
    ]);
  });
  it('DSUM', (done) => {
    _do([
      excel('A1', 'Tree'),
      excel('A10', 'Pear'),
      excel('A11', 'Apple'),
      excel('A12', 'Formula'),
      excel('A13', '=DSUM(A5:E11),"Profit",A1:A2)'),
      excel('A14', '=DSUM(A5:E11,"Profit", A1:F3)'),
      excel('A2', '="=Apple"'),
      excel('A3', '="=Pear"'),
      excel('A5', 'Tree'),
      excel('A6', 'Apple'),
      excel('A7', 'Pear'),
      excel('A8', 'Cherry'),
      excel('A9', 'Apple'),
      excel('B1', 'Height'),
      excel('B10', '9'),
      excel('B11', '8'),
      excel('B12', 'Description'),
      excel('B13', 'The total profit from apple trees (rows 6, 9, and 10).'),
      excel('B14', 'The total profit from apple trees with a height between 10 and 16 feet, and all pear trees (rows 7, 9, and 10).'),
      excel('B2', '>10'),
      excel('B5', 'Height'),
      excel('B6', '18'),
      excel('B7', '12'),
      excel('B8', '13'),
      excel('B9', '14'),
      excel('C1', 'Age'),
      excel('C10', '8'),
      excel('C11', '9'),
      excel('C12', 'Result'),
      excel('C5', 'Age'),
      excel('C6', '20'),
      excel('C7', '12'),
      excel('C8', '14'),
      excel('C9', '15'),
      excel('D1', 'Yield'),
      excel('D10', '8'),
      excel('D11', '6'),
      excel('D5', 'Yield'),
      excel('D6', '14'),
      excel('D7', '10'),
      excel('D8', '9'),
      excel('D9', '10'),
      excel('E1', 'Profit'),
      excel('E10', '$77'),
      excel('E11', '$45'),
      excel('E5', 'Profit'),
      excel('E6', '$105'),
      excel('E7', '$96'),
      excel('E8', '$105'),
      excel('E9', '$75'),
      excel('F1', 'Height'),
      excel('F2', '<16'),

      shouldBe('A13', valueS('$225')),
      shouldBe('A14', valueS('$248')),

      exec(done)
    ]);
  });
  it('DVAR', (done) => {
    _do([
      excel('A1', 'Tree'),
      excel('A10', 'Apple'),
      excel('A11', 'Formula'),
      excel('A12', '=DVAR(A4:E10, "Yield", A1:A3)'),
      excel('A2', '="=Apple"'),
      excel('A3', '="=Pear"'),
      excel('A4', 'Tree'),
      excel('A5', 'Apple'),
      excel('A6', 'Pear'),
      excel('A7', 'Cherry'),
      excel('A8', 'Apple'),
      excel('A9', 'Pear'),
      excel('B1', 'Height'),
      excel('B10', '8'),
      excel('B11', 'Description'),
      excel('B12', 'The estimated variance in the yield of apple and pear trees if the data in the database is only a sample of the total orchard population.'),
      excel('B2', '>10'),
      excel('B4', 'Height'),
      excel('B5', '18'),
      excel('B6', '12'),
      excel('B7', '13'),
      excel('B8', '14'),
      excel('B9', '9'),
      excel('C1', 'Age'),
      excel('C10', '9'),
      excel('C11', 'Result'),
      excel('C4', 'Age'),
      excel('C5', '20'),
      excel('C6', '12'),
      excel('C7', '14'),
      excel('C8', '15'),
      excel('C9', '8'),
      excel('D1', 'Yield'),
      excel('D10', '6'),
      excel('D4', 'Yield'),
      excel('D5', '14'),
      excel('D6', '10'),
      excel('D7', '9'),
      excel('D8', '10'),
      excel('D9', '8'),
      excel('E1', 'Profit'),
      excel('E10', '$45'),
      excel('E4', 'Profit'),
      excel('E5', '$105'),
      excel('E6', '$96'),
      excel('E7', '$105'),
      excel('E8', '$75'),
      excel('E9', '$77'),
      excel('F1', 'Height'),
      excel('F2', '<16'),

      shouldBe('A12', valueD(8.8)),

      exec(done)
    ]);
  });
  it('DVARP', (done) => {
    _do([
      excel('A1', 'Tree'),
      excel('A10', 'Apple'),
      excel('A11', 'Formula'),
      excel('A12', '=DVARP(A4:E10, "Yield", A1:A3)'),
      excel('A2', '="=Apple"'),
      excel('A3', '="=Pear"'),
      excel('A4', 'Tree'),
      excel('A5', 'Apple'),
      excel('A6', 'Pear'),
      excel('A7', 'Cherry'),
      excel('A8', 'Apple'),
      excel('A9', 'Pear'),
      excel('B1', 'Height'),
      excel('B10', '8'),
      excel('B11', 'Description'),
      excel('B12', 'The true variance in the yield of apple and pear trees if the data in the database is the entire orchard population.'),
      excel('B2', '>10'),
      excel('B4', 'Height'),
      excel('B5', '18'),
      excel('B6', '12'),
      excel('B7', '13'),
      excel('B8', '14'),
      excel('B9', '9'),
      excel('C1', 'Age'),
      excel('C10', '9'),
      excel('C11', 'Result'),
      excel('C4', 'Age'),
      excel('C5', '20'),
      excel('C6', '12'),
      excel('C7', '14'),
      excel('C8', '15'),
      excel('C9', '8'),
      excel('D1', 'Yield'),
      excel('D10', '6'),
      excel('D4', 'Yield'),
      excel('D5', '14'),
      excel('D6', '10'),
      excel('D7', '9'),
      excel('D8', '10'),
      excel('D9', '8'),
      excel('E1', 'Profit'),
      excel('E10', '$45'),
      excel('E4', 'Profit'),
      excel('E5', '$105'),
      excel('E6', '$96'),
      excel('E7', '$105'),
      excel('E8', '$75'),
      excel('E9', '$77'),
      excel('F1', 'Height'),
      excel('F2', '<16'),

      shouldBe('A12', valueD(7.04)),

      exec(done)
    ]);
  });
  it('DATE', (done) => {
    _do([
      excel('A1', 'Year'),
      excel('A2', '2011'),
      excel('A3', 'Data'),
      excel('A4', '20111125'),
      excel('A5', 'Formula'),
      excel('A6', '=DATE(A2,B2,C2)'),
      excel('A7', '=DATE(YEAR(TODAY()),12,31)'),
      excel('A8', '=DATE(LEFT(A4,4),MID(A4,5,2), RIGHT(A4,2))'),
      excel('B1', 'Month'),
      excel('B2', '1'),
      excel('B5', 'Description'),
      excel('B6', 'Serial date for the date derived by using cells A2, B2, and C2 as the arguments for the DATE function.'),
      excel('B7', 'Serial date for the last day of the current year.'),
      excel('B8', 'A formula that converts the text string in A4 (20111125) representing a date in a "YYYYMMDD" format to a date.'),
      excel('C1', 'Day'),
      excel('C2', '1'),
      excel('C5', 'Result'),

      shouldBe('A6', valueI(40544)),
      shouldBe('A7', valueI(41274)),
      shouldBe('A8', valueS('11/25/2011')),

      exec(done)
    ]);
  });
  it('DATEVALUE', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A10', '=DATEVALUE(A2 & "/" & A3 & "/" & A4)'),
      excel('A2', '11'),
      excel('A3', '3'),
      excel('A4', '2011'),
      excel('A5', 'Formula'),
      excel('A6', '=DATEVALUE("8/22/2011")'),
      excel('A7', '=DATEVALUE("22-MAY-2011")'),
      excel('A8', '=DATEVALUE("2011/02/23")'),
      excel('A9', '=DATEVALUE("5-JUL")'),
      excel('B10', 'Serial number of a date created by combining the values in cells A2, A3, and A4.'),
      excel('B5', 'Description'),
      excel('B6', 'Serial number of a date entered as text.'),
      excel('B7', 'Serial number of a date entered as text.'),
      excel('B8', 'Serial number of a date entered as text.'),
      excel('B9', 'Serial number of a date entered as text, using the 1900 date system, and assuming the computer\'s built-in clock returns 2011 as the current year.'),
      excel('C5', 'Result'),

      shouldBe('A10', valueI(40850)),
      shouldBe('A6', valueI(40777)),
      shouldBe('A7', valueI(40685)),
      shouldBe('A8', valueI(40597)),
      shouldBe('A9', valueI(39634)),

      exec(done)
    ]);
  });
  it('DAY', (done) => {
    _do([
      excel('A1', 'Date'),
      excel('A2', '15-Apr-11'),
      excel('A3', 'Formula'),
      excel('A4', '=DAY(A2)'),
      excel('B3', 'Description (Result)'),
      excel('B4', 'Day of the date in cell A2 (15)'),
      excel('C3', 'Result'),

      shouldBe('A4', valueI(15)),

      exec(done)
    ]);
  });
  it('DAYS', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A2', '12/31/2011'),
      excel('A3', '1/1/2011'),
      excel('A4', 'Formula'),
      excel('A5', '=DAYS("3/15/11","2/1/11")'),
      excel('A6', '=DAYS(A2,A3)'),
      excel('B4', 'Description'),
      excel('B5', 'Finds the number of days between the end date (3/15/11) and end date (2/1/11). When you enter a date directly in the function, you need to enclose it in quotation marks. Result is 42.'),
      excel('B6', 'Finds the number of days between the end date in A2 and the start date in A3 (364).'),
      excel('C4', 'Result'),

      shouldBe('A5', valueI(42)),
      shouldBe('A6', valueI(364)),

      exec(done)
    ]);
  });
  it('DAYS360', (done) => {
    _do([
      excel('A1', 'Dates'),
      excel('A2', '1-Jan-11'),
      excel('A3', '30-Jan-11'),
      excel('A4', '1-Feb-11'),
      excel('A5', '31-Dec-11'),
      excel('A6', 'Formula'),
      excel('A7', '=DAYS360(A3,A4)'),
      excel('A8', '=DAYS360(A2,A5)'),
      excel('A9', '=DAYS360(A2,A4)'),
      excel('B6', 'Description'),
      excel('B7', 'Number of days between 1/30/2011 and 2/1/2011, based on a 360-day year.'),
      excel('B8', 'Number of days between 1/1/2011 and 12/31/2011, based on a 360-day year.'),
      excel('B9', 'Number of days between 1/1/2011 and 2/1/2011, based on a 360-day year.'),
      excel('C6', 'Result'),

      shouldBe('A7', valueI(1)),
      shouldBe('A8', valueI(360)),
      shouldBe('A9', valueI(30)),

      exec(done)
    ]);
  });
  it('EDATE', (done) => {
    _do([
      excel('A1', 'Date'),
      excel('A2', '15-Jan-11'),
      excel('A3', 'Formula'),
      excel('A4', '=EDATE(A2,1)'),
      excel('A5', '=EDATE(A2,-1)'),
      excel('A6', '=EDATE(A2,2)'),
      excel('B3', 'Description'),
      excel('B4', 'The date, one month after the date above'),
      excel('B5', 'The date, one month before the date above'),
      excel('B6', 'The date, two months after the date above'),
      excel('C3', 'Result'),

      shouldBe('A4', valueS('15-Feb-11')),
      shouldBe('A5', valueS('15-Dec-10')),
      shouldBe('A6', valueS('15-Mar-11')),

      exec(done)
    ]);
  });
  it('EOMONTH', (done) => {
    _do([
      excel('A1', 'Date'),
      excel('A2', '1-Jan-11'),
      excel('A3', 'Formula'),
      excel('A4', '=EOMONTH(A2,1)'),
      excel('A5', '=EOMONTH(A2,-3)'),
      excel('B3', 'Description'),
      excel('B4', 'Date of the last day of the month, one month after the date in A2.'),
      excel('B5', 'Date of the last day of the month, three months before the date in A2.'),
      excel('C3', 'Result'),

      shouldBe('A4', valueS('2/28/2011')),
      shouldBe('A5', valueS('10/31/2010')),

      exec(done)
    ]);
  });
  it('HOUR', (done) => {
    _do([
      excel('A1', 'Time'),
      excel('A2', '0.75'),
      excel('A3', '7/18/2011 7:45'),
      excel('A4', '4/21/2012'),
      excel('A5', 'Formula'),
      excel('A6', '=HOUR(A2)'),
      excel('A7', '=HOUR(A3)'),
      excel('A8', '=HOUR(A4)'),
      excel('B5', 'Description'),
      excel('B6', 'Returns 75% of 24 hours'),
      excel('B7', 'Returns the hour portion of the date/time value.'),
      excel('B8', 'A date with no time portion specified is considered 12:00 AM, or 0 hours.'),
      excel('C5', 'Result'),

      shouldBe('A6', valueI(18)),
      shouldBe('A7', valueI(7)),
      shouldBe('A8', valueI(0)),

      exec(done)
    ]);
  });
  it('ISOWEEKNUM', (done) => {
    _do([
      excel('A1', 'Date'),
      excel('A2', '3/9/2012'),
      excel('A3', 'Formula'),
      excel('A4', '=ISOWEEKNUM(A2)'),
      excel('B3', 'Description'),
      excel('B4', 'Number of the week in the year that 3/9/2012 occurs, based on weeks beginning on the default, Monday (10).'),
      excel('C3', 'Result'),

      shouldBe('A4', valueI(10)),

      exec(done)
    ]);
  });
  it('MINUTE', (done) => {
    _do([
      excel('A1', 'Time'),
      excel('A2', '12:45:00 PM'),
      excel('A3', 'Formula'),
      excel('A4', '=MINUTE(A2)'),
      excel('B3', 'Description'),
      excel('B4', 'Minute portion of the time in A2.'),
      excel('C3', 'Result'),

      shouldBe('A4', valueI(45)),

      exec(done)
    ]);
  });
  it('MONTH', (done) => {
    _do([
      excel('A1', 'Date'),
      excel('A2', '15-Apr-11'),
      excel('A3', 'Formula'),
      excel('A4', '=MONTH(A2)'),
      excel('B3', 'Description'),
      excel('B4', 'Month of the date in cell A2'),
      excel('C3', 'Result'),

      shouldBe('A4', valueI(4)),

      exec(done)
    ]);
  });
  it('NETWORKDAYS', (done) => {
    _do([
      excel('A1', 'Date'),
      excel('A10', '=NETWORKDAYS(A2,A3,A4:A6)'),
      excel('A2', '10/1/2012'),
      excel('A3', '3/1/2013'),
      excel('A4', '11/22/2012'),
      excel('A5', '12/4/2012'),
      excel('A6', '1/21/2013'),
      excel('A7', 'Formula'),
      excel('A8', '=NETWORKDAYS(A2,A3)'),
      excel('A9', '=NETWORKDAYS(A2,A3,A4)'),
      excel('B1', 'Description'),
      excel('B10', 'Number of workdays between the start (10/1/2012) and end date (3/1/2013), with the three holidays as non-working days.'),
      excel('B2', 'Start date of project'),
      excel('B3', 'End date of project'),
      excel('B4', 'Holiday'),
      excel('B5', 'Holiday'),
      excel('B6', 'Holiday'),
      excel('B7', 'Description'),
      excel('B8', 'Number of workdays between the start (10/1/2012) and end date (3/1/2013).'),
      excel('B9', 'Number of workdays between the start (10/1/2012) and end date (3/1/2013), with the 11/22/2012 holiday as a non-working day.'),
      excel('C7', 'Result'),

      shouldBe('A10', valueI(107)),
      shouldBe('A8', valueI(110)),
      shouldBe('A9', valueI(109)),

      exec(done)
    ]);
  });
  it('NETWORKDAYSINTL', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=NETWORKDAYS.INTL(DATE(2006,1,1),DATE(2006,1,31))'),
      excel('A3', '=NETWORKDAYS.INTL(DATE(2006,2,28),DATE(2006,1,31))'),
      excel('A4', '=NETWORKDAYS.INTL(DATE(2006,1,1),DATE(2006,2,1),7,{"2006/1/2","2006/1/16"})'),
      excel('A5', '=NETWORKDAYS.INTL(DATE(2006,1,1),DATE(2006,2,1),"0010001",{"2006/1/2","2006/1/16"})'),
      excel('B1', 'Description'),
      excel('B2', 'Results in 22 future workdays. Subtracts 9 nonworking weekend days (5 Saturdays and 4 Sundays) from the 31 total days between the two dates. By default, Saturday and Sunday are considered non-working days.'),
      excel('B3', 'Results in -21, which is 21 workdays in the past.'),
      excel('B4', 'Results in 22 future workdays by sutracting 10 nonworking days (4 Fridays, 4 Saturdays, 2 Holidays) from the 32 days between Jan 1 2006 and Feb 1 2006. Uses the 7 argument for weekend, which is Friday and Saturday. There are also two holidays in this time period.'),
      excel('B5', 'Results in 22 future workdays. Same time period as example directly above, but with Sunday and Wednesday as weekend days.'),
      excel('C1', 'Result'),

      shouldBe('A2', valueI(22)),
      shouldBe('A3', valueI(-21)),
      shouldBe('A4', valueI(22)),
      shouldBe('A5', valueI(20)),

      exec(done)
    ]);
  });
  it('NOW', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=NOW()'),
      excel('A3', '=NOW()-0.5'),
      excel('A4', '=NOW()+7'),
      excel('A5', '=NOW()-2.25'),
      excel('B1', 'Description'),
      excel('B2', 'Returns the current date and time.'),
      excel('B3', 'Returns the date and time 12 hours ago (-0.5 days ago).'),
      excel('B4', 'Returns the date and time 7 days in the future.'),
      excel('B5', 'Returns the date and time 2 days and 6 hours ago (-2.25 days ago).'),
      excel('C1', 'Result'),

      shouldBe('A2', valueS('11/6/2011 19:03')),
      shouldBe('A3', valueS('11/6/2011 7:03')),
      shouldBe('A4', valueS('11/13/2011 19:03')),
      shouldBe('A5', valueS('11/4/2011 13:03')),

      exec(done)
    ]);
  });
  it('SECOND', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A2', 'Time'),
      excel('A3', '4:48:18 PM'),
      excel('A4', '4:48 PM'),
      excel('A5', 'Formula'),
      excel('A6', '=SECOND(A3)'),
      excel('A7', '=SECOND(A4)'),
      excel('B5', 'Description'),
      excel('B6', 'Seconds in the first time (18)'),
      excel('B7', 'Seconds in the second time (0)'),
      excel('C5', 'Result'),

      shouldBe('A6', valueI(18)),
      shouldBe('A7', valueI(0)),

      exec(done)
    ]);
  });
  it('TIME', (done) => {
    _do([
      excel('A1', 'Hour'),
      excel('A2', '12'),
      excel('A3', '16'),
      excel('A4', 'Formula'),
      excel('A5', '=TIME(A2,B2,C2)'),
      excel('A6', '=TIME(A3,B3,C3)'),
      excel('B1', 'Minute'),
      excel('B2', '0'),
      excel('B3', '48'),
      excel('B4', 'Description'),
      excel('B5', 'Decimal part of a day, for the time specified in row 2 (12 hours, 0, minutes, 0 seconds)'),
      excel('B6', 'Decimal part of a day, for the time specified in row 3 (16 hours, 48 minutes, 10 seconds)'),
      excel('C1', 'Second'),
      excel('C2', '0'),
      excel('C3', '10'),
      excel('C4', 'Result'),

      shouldBe('A5', valueD(0.5)),
      shouldBe('A6', valueD(0.7001157)),

      exec(done)
    ]);
  });
  it('TIMEVALUE', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=TIMEVALUE("2:24 AM")'),
      excel('A3', '=TIMEVALUE("22-Aug-2011 6:35 AM")'),
      excel('B1', 'Description'),
      excel('B2', 'Decimal part of a day, with only the time specified.'),
      excel('B3', 'Decimal part of a day, with date and time specified.'),
      excel('C1', 'Result'),

      shouldBe('A2', valueD(0.10)),
      shouldBe('A3', valueD(0.2743)),

      exec(done)
    ]);
  });
  it('TODAY', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=TODAY()'),
      excel('A3', '=TODAY()+5'),
      excel('A4', '=DATEVALUE("1/1/2030")-TODAY()'),
      excel('A5', '=DAY(TODAY())'),
      excel('A6', '=MONTH(TODAY())'),
      excel('B1', 'Description'),
      excel('B2', 'Returns the current date.'),
      excel('B3', 'Returns the current date plus 5 days. For example, if the current date is 1/1/2012, this formula returns 1/6/2012.'),
      excel('B4', 'Returns the number of days between the current date and 1/1/2030. Note that cell A4 must be formatted as General or Number for the result to display correctly.'),
      excel('B5', 'Returns the current day of the month (1 - 31).'),
      excel('B6', 'Returns the current month of the year (1 - 12). For example, if the current month is May, this formula returns 5.'),
      excel('C1', 'Result'),

      shouldBe('A2', valueS('12/1/2011')),
      shouldBe('A3', valueS('12/6/2011')),
      shouldBe('A4', valueS('1/31/1918')),
      shouldBe('A5', valueI(1)),
      shouldBe('A6', valueI(12)),

      exec(done)
    ]);
  });
  it('WEEKDAY', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A2', '2/14/2008'),
      excel('A3', 'Formula'),
      excel('A4', '=WEEKDAY(A2)'),
      excel('A5', '=WEEKDAY(A2, 2)'),
      excel('A6', '=WEEKDAY(A2, 3)'),
      excel('B3', 'Description (Result)'),
      excel('B4', 'Day of the week, with numbers 1 (Sunday) through 7 (Saturday) (5)'),
      excel('B5', 'Day of the week, with numbers 1 (Monday) through 7 (Sunday) (4)'),
      excel('B6', 'Day of the week, with numbers 0 (Monday) through 6 (Sunday) (3)'),
      excel('C3', 'Result'),

      shouldBe('A4', valueI(5)),
      shouldBe('A5', valueI(4)),
      shouldBe('A6', valueI(3)),

      exec(done)
    ]);
  });
  it('WEEKNUM', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A2', '3/9/2012'),
      excel('A3', 'Formula'),
      excel('A4', '=WEEKNUM(A2)'),
      excel('A5', '=WEEKNUM(A2,2)'),
      excel('B3', 'Description'),
      excel('B4', 'Number of the week in the year that 3/9/2012 occurs, based on weeks beginning on Sunday (default).'),
      excel('B5', 'Number of the week in the year that 3/9/2012 occurs, based on a week beginning on Monday (the second argument, 2).'),
      excel('C3', 'Result'),

      shouldBe('A4', valueI(10)),
      shouldBe('A5', valueI(11)),

      exec(done)
    ]);
  });
  it('WORKDAY', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A2', '10/1/2008'),
      excel('A3', '151'),
      excel('A4', '11/26/2008'),
      excel('A5', '12/4/2008'),
      excel('A6', '1/21/2009'),
      excel('A7', 'Formula'),
      excel('A8', '=WORKDAY(A2,A3)'),
      excel('A9', '=WORKDAY(A2,A3,A4:A6)'),
      excel('B2', 'Start date'),
      excel('B3', 'Days to completion'),
      excel('B4', 'Holiday'),
      excel('B5', 'Holiday'),
      excel('B6', 'Holiday'),
      excel('B7', 'Description (Result)'),
      excel('B8', 'Date 151 workdays from the start date (4/30/2009)'),
      excel('B9', 'Date 151 workdays from the start date, excluding holidays (5/5/2009)'),
      excel('C7', 'Result'),

      shouldBe('A8', valueS('4/30/2009')),
      shouldBe('A9', valueS('5/5/2009')),

      exec(done)
    ]);
  });
  it('WORKDAYINTL', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=WORKDAY.INTL(DATE(2012,1,1),30,0)'),
      excel('A3', '=WORKDAY.INTL(DATE(2012,1,1),90,11)'),
      excel('A4', '=TEXT(WORKDAY.INTL(DATE(2012,1,1),30,17),"m/dd/yyyy")'),
      excel('B1', 'Description'),
      excel('B2', 'Using a 0 for the Weekend argument results in a #NUM! error.'),
      excel('B3', 'Finds the date 90 workdays from 1/1/2012, counting only Sundays as a weekend day (Weekend argument is 11).'),
      excel('B4', 'Uses the TEXT function to format the resulting serial number (40944) in a "m/dd/yyyy" format. Finds the date 30 workdays from 1/1/2012, counting only Saturdays as a weekend day (Weekend argument is 17).'),
      excel('C1', 'Live Result'),

      shouldBe('A2', valueS('#NUM!')),
      shouldBe('A3', valueI(41013)),
      shouldBe('A4', valueS('2/05/2012')),

      exec(done)
    ]);
  });
  it('YEAR', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A2', 'Date'),
      excel('A3', '7/5/2008'),
      excel('A4', '7/5/2010'),
      excel('A5', 'Formula'),
      excel('A6', '=YEAR(A3)'),
      excel('A7', '=YEAR(A4)'),
      excel('B5', 'Description (Result)'),
      excel('B6', 'Year of the date in cell A3 (2008)'),
      excel('B7', 'Year of the date in cell A4 (2010)'),
      excel('C5', 'Result'),

      shouldBe('A6', valueI(2008)),
      shouldBe('A7', valueI(2010)),

      exec(done)
    ]);
  });
  it('YEARFRAC', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A2', '1/1/2012'),
      excel('A3', '7/30/2012'),
      excel('A4', 'Formula'),
      excel('A5', '=YEARFRAC(A2,A3)'),
      excel('A6', '=YEARFRAC(A2,A3,1)'),
      excel('A7', '=YEARFRAC(A2,A3,3)'),
      excel('B1', 'Description'),
      excel('B2', 'Start date'),
      excel('B3', 'End date'),
      excel('B4', 'Description'),
      excel('B5', 'Fraction of the year between 1/1/2012 and 7/30/12, omitting the Basis argument.'),
      excel('B6', 'Fraction between same dates, using the Actual/Actual basis argument. Because 2012 is a Leap year, it has a 366 day basis.'),
      excel('B7', 'Fraction between same dates, using the Actual/365 basis argument. Uses a 365 day basis.'),
      excel('C4', 'Result'),

      shouldBe('A5', valueD(0.58055556)),
      shouldBe('A6', valueD(0.57650273)),
      shouldBe('A7', valueD(0.57808219)),

      exec(done)
    ]);
  });
  it('BESSELI', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=BESSELI(1.5, 1)'),
      excel('B1', 'Description'),
      excel('B2', 'Modified Bessel function at 1.5 with an order of 1.'),
      excel('C1', 'Result'),

      shouldBe('A2', valueD(0.981666428)),

      exec(done)
    ]);
  });
  it('BESSELJ', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=BESSELJ(1.9, 2)'),
      excel('B1', 'Description'),
      excel('B2', 'Bessel function at 1.9 with an order of 2.'),
      excel('C1', 'Result'),

      shouldBe('A2', valueD(0.329925829)),

      exec(done)
    ]);
  });
  it('BESSELK', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=BESSELK(1.5, 1)'),
      excel('B1', 'Description'),
      excel('B2', 'Modified Bessel function at 1.5 with an order of 1.'),
      excel('C1', 'Result'),

      shouldBe('A2', valueD(0.277387804)),

      exec(done)
    ]);
  });
  it('BESSELY', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=BESSELY(2.5, 1)'),
      excel('B1', 'Description'),
      excel('B2', 'Weber\'s Bessel function at 2.5 and an order of 1'),
      excel('C1', 'Result'),

      shouldBe('A2', valueD(0.145918138)),

      exec(done)
    ]);
  });
  it('BIN2DEC', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=BIN2DEC(1100100)'),
      excel('A3', '=BIN2DEC(1111111111)'),
      excel('B1', 'Description'),
      excel('B2', 'Converts binary 1100100 to decimal'),
      excel('B3', 'Converts binary 1111111111 to decimal'),
      excel('C1', 'Result'),

      shouldBe('A2', valueI(100)),
      shouldBe('A3', valueI(-1)),

      exec(done)
    ]);
  });
  it('BIN2HEX', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=BIN2HEX(11111011, 4)'),
      excel('A3', '=BIN2HEX(1110)'),
      excel('A4', '=BIN2HEX(1111111111)'),
      excel('B1', 'Description'),
      excel('B2', 'Converts binary 11111011 to hexadecimal with 4 characters'),
      excel('B3', 'Converts binary 1110 to hexadecimal'),
      excel('B4', 'Converts binary 1111111111 to hexadecimal'),
      excel('C1', 'Result'),

      shouldBe('A2', valueS('00FB')),
      shouldBe('A3', valueS('E')),
      shouldBe('A4', valueS('FFFFFFFFFF')),

      exec(done)
    ]);
  });
  it('BIN2OCT', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=BIN2OCT(1001, 3)'),
      excel('A3', '=BIN2OCT(1100100)'),
      excel('A4', '=BIN2OCT(1111111111)'),
      excel('B1', 'Description'),
      excel('B2', 'Converts binary 1001 to octal with 3 characters'),
      excel('B3', 'Converts binary 1100100 to octal'),
      excel('B4', 'Converts binary 1111111111 to octal'),
      excel('C1', 'Result'),

      shouldBe('A2', valueI(11)),
      shouldBe('A3', valueI(144)),
      shouldBe('A4', valueI(7777777777)),

      exec(done)
    ]);
  });
  it('BITAND', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=BITAND(1,5)'),
      excel('A4', '=BITAND(13,25)'),
      excel('B1', 'Description'),
      excel('B2', 'Compares the binary representations of 1 and 5.'),
      excel('B4', 'Compares the binary representations of 13 and 25.'),
      excel('C1', 'Result'),
      excel('C6', 'Decimal number'),
      excel('C7', '13'),
      excel('C8', '25'),
      excel('D1', 'How it works'),
      excel('D2', 'The binary representation of 1 is 1, and the binary representation of 5 is 101. Their bits match only at the rightmost position. This is returned as 2^0, or 1.'),
      excel('D4', 'The binary representation of 13 is 1101, and the binary representation of 25 is 11001. Their bits match at the rightmost position and at the position fourth from the right. This is returned as (2^0)+ (2^3), or 9.'),
      excel('D6', 'Binary representation'),
      excel('D7', '1101'),
      excel('D8', '11001'),

      shouldBe('A2', valueI(1)),
      shouldBe('A4', valueI(9)),

      exec(done)
    ]);
  });
  it('BITLSHIFT', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=BITLSHIFT(4,2)'),
      excel('B1', 'Description'),
      excel('B2', 'Shifts bits left for the number by adding zeros (0) to the right of the number represented in binary. The number returned is represented in decimal.'),
      excel('C1', 'Result'),
      excel('D1', 'How it works'),
      excel('D2', '4 is represented as 100 in binary. Adding two 0 digits to the right results in 10000, which is 16 in decimal.'),

      shouldBe('A2', valueI(16)),

      exec(done)
    ]);
  });
  it('BITOR', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=BITOR(23,10)'),
      excel('B1', 'Description'),
      excel('B2', 'Compares the bit positions for the binary representations of the two numbers, and if either position contains 1, returns 2 raised to a power, depending on bit position. Then, those numbers are summed.'),
      excel('C1', 'Result'),
      excel('D1', 'How it works'),
      excel('D2', 'The number 23 is 10111 in binary, and 10 is 1010. The value 1 is found in either position at all 5 positions of either of the two numbers. You can express 1010 as 01010 so that both numbers have the same number of digits. The numbers 2^0, 2^1, 2^2, 2^3, and 2^4 are summed, for a total of 31.'),
      excel('D3', '23 = 10111'),
      excel('D4', '10 = 01010'),
      excel('D5', 'Test: Is 1 found at any of the 5 positions?'),
      excel('D6', 'yyyyy'),
      excel('D7', '1+2+4+8+16=31'),

      shouldBe('A2', valueI(31)),

      exec(done)
    ]);
  });
  it('BITRSHIFT', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=BITRSHIFT(13,2)'),
      excel('B1', 'Description'),
      excel('B2', 'Shifts bits right for the number by stripping the specified rightmost digits of the number represented in binary. The number returned is represented in decimal.'),
      excel('C1', 'Result'),
      excel('D1', 'How it works'),
      excel('D2', '13 is represented as 1101 in binary. Stripping the rightmost two digits results in 11, which is 3 in decimal.'),

      shouldBe('A2', valueI(3)),

      exec(done)
    ]);
  });
  it('BITXOR', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=BITXOR(5,3)'),
      excel('B1', 'Description'),
      excel('B2', 'Returns the total of the bitwise "Exclusive Or" comparision at each bit position.'),
      excel('C1', 'Result'),
      excel('D1', 'How it works'),
      excel('D10', '6'),
      excel('D2', 'The number 5 is 101 in binary, and 3 is 11. You can express 11 as 011 so that both numbers have 3 digits. A bitwise \'Exclusive Or\' comparison checks to see if both digits at each position are not equal and, if true, returns a positive value for that position.'),
      excel('D4', 'Test: At which positions are the digits not equal?'),
      excel('D5', '5 in binary = 101'),
      excel('D6', '3 in binary = 011'),
      excel('D7', 'yyn'),
      excel('D8', '(1*(2^2))+(1*(2^1))+(1*0)'),
      excel('D9', '4+2+0'),

      shouldBe('A2', valueI(6)),

      exec(done)
    ]);
  });
  it('COMPLEX', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=COMPLEX(3,4)'),
      excel('A3', '=COMPLEX(3,4,"j")'),
      excel('A4', '=COMPLEX(0,1)'),
      excel('A5', '=COMPLEX(1,0)'),
      excel('B1', 'Description'),
      excel('B2', 'Complex number with 3 and 4 as the real and imaginary coefficients'),
      excel('B3', 'Complex number with 3 and 4 as the real and imaginary coefficients, and j as the suffix'),
      excel('B4', 'Complex number with 0 and 1 as the real and imaginary coefficients'),
      excel('B5', 'Complex number with 1 and 0 as the real and imaginary coefficients'),
      excel('C1', 'Result'),

      shouldBe('A2', valueS('3+4i')),
      shouldBe('A3', valueS('3+4j')),
      shouldBe('A4', valueS('i')),
      shouldBe('A5', valueI(1)),

      exec(done)
    ]);
  });
  it('CONVERT', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=CONVERT(1, "lbm", "kg")'),
      excel('A3', '=CONVERT(68, "F", "C")'),
      excel('A4', '=CONVERT(2.5, "ft", "sec")'),
      excel('A5', '=CONVERT(CONVERT(100,"ft","m"),"ft","m")'),
      excel('B1', 'Description'),
      excel('B2', 'Converts 1 pound mass to kilograms.'),
      excel('B3', 'Converts 68 degrees Fahrenheit to Celsius.'),
      excel('B4', 'Data types are not the same, so an error is returned.'),
      excel('B5', 'Converts 100 square feet into square meters.'),
      excel('C1', 'Result'),

      shouldBe('A2', valueD(0.4535924)),
      shouldBe('A3', valueI(20)),
      shouldBe('A4', valueS('#N/A')),
      shouldBe('A5', valueD(9.290304)),

      exec(done)
    ]);
  });
  it('DEC2BIN', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=DEC2BIN(9, 4)'),
      excel('A3', '=DEC2BIN(-100)'),
      excel('B1', 'Description'),
      excel('B2', 'Converts decimal 9 to binary with 4 characters.'),
      excel('B3', 'Converts decimal -100 to binary'),
      excel('C1', 'Result'),

      shouldBe('A2', valueI(1001)),
      shouldBe('A3', valueI(1110011100)),

      exec(done)
    ]);
  });
  it('DEC2HEX', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=DEC2HEX(100, 4)'),
      excel('A3', '=DEC2HEX(-54)'),
      excel('A4', '=DEC2HEX(28)'),
      excel('A5', '=DEC2HEX(64,1)'),
      excel('B1', 'Description'),
      excel('B2', 'Converts decimal value 100 to hexadecimal with 4 characters ("padded" with two leading zeros).'),
      excel('B3', 'Converts decimal value -54 to hexadecimal'),
      excel('B4', 'Converts decimal value 28 to hexadecimal.'),
      excel('B5', 'Returns the #NUM! error value because the result (40) requires 2 character places.'),
      excel('C1', 'Result'),

      shouldBe('A2', valueI(64)),
      shouldBe('A3', valueS('FFFFFFFFCA')),
      shouldBe('A4', valueS('1C')),
      shouldBe('A5', valueS('#NUM!')),

      exec(done)
    ]);
  });
  it('DEC2OCT', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=DEC2OCT(58, 3)'),
      excel('A3', '=DEC2OCT(-100)'),
      excel('B1', 'Description'),
      excel('B2', 'Converts decimal 58 to octal'),
      excel('B3', 'Converts decimal to octal.'),
      excel('C1', 'Result'),

      shouldBe('A2', valueI(72)),
      shouldBe('A3', valueI(7777777634)),

      exec(done)
    ]);
  });
  it('DELTA', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=DELTA(5, 4)'),
      excel('A3', '=DELTA(5, 5)'),
      excel('A4', '=DELTA(0.5, 0)'),
      excel('B1', 'Description'),
      excel('B2', 'Checks whether 5 equals 4'),
      excel('B3', 'Checks whether 5 equals 5'),
      excel('B4', 'Checks whether 0.5 equals 0'),
      excel('C1', 'Result'),

      shouldBe('A2', valueI(0)),
      shouldBe('A3', valueI(1)),
      shouldBe('A4', valueI(0)),

      exec(done)
    ]);
  });
  it('ERF', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=ERF(0.745)'),
      excel('A3', '=ERF(1)'),
      excel('B1', 'Description'),
      excel('B2', 'Error function integrated between 0 and 0.74500'),
      excel('B3', 'Error function integrated between 0 and 1.'),
      excel('C1', 'Result'),

      shouldBe('A2', valueD(0.70792892)),
      shouldBe('A3', valueD(0.84270079)),

      exec(done)
    ]);
  });
  it('ERFPRECISE', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=ERF.PRECISE(0.745)'),
      excel('A3', '=ERF.PRECISE(1)'),
      excel('B1', 'Description'),
      excel('B2', 'Error function integrated between 0 and 0.74500'),
      excel('B3', 'Error function integrated between 0 and 1.'),
      excel('C1', 'Result'),

      shouldBe('A2', valueD(0.70792892)),
      shouldBe('A3', valueD(0.84270079)),

      exec(done)
    ]);
  });
  it('ERFC', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=ERFC(1)'),
      excel('B1', 'Description'),
      excel('B2', 'Complementary ERF function of 1.'),
      excel('C1', 'Result'),

      shouldBe('A2', valueD(0.15729921)),

      exec(done)
    ]);
  });
  it('ERFCPRECISE', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=ERFC.PRECISE(1)'),
      excel('B1', 'Description'),
      excel('B2', 'Complementary ERF function of 1.'),
      excel('C1', 'Result'),

      shouldBe('A2', valueD(0.15729921)),

      exec(done)
    ]);
  });
  it('GESTEP', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=GESTEP(5, 4)'),
      excel('A3', '=GESTEP(5, 5)'),
      excel('A4', '=GESTEP(-4, -5)'),
      excel('A5', '=GESTEP(-1)'),
      excel('B1', 'Description'),
      excel('B2', 'Checks whether 5 is greater than or equal to  the step value, 4.'),
      excel('B3', 'Checks whether 5 is greater than or equal to the step value, 5.'),
      excel('B4', 'Checks whether -4 is greater than or equal to the step value, -5.'),
      excel('B5', 'Checks whether -1 is greater than the default step value, 0.'),
      excel('C1', 'Result'),

      shouldBe('A2', valueI(1)),
      shouldBe('A3', valueI(1)),
      shouldBe('A4', valueI(1)),
      shouldBe('A5', valueI(0)),

      exec(done)
    ]);
  });
  it('HEX2BIN', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=HEX2BIN("F", 8)'),
      excel('A3', '=HEX2BIN("B7")'),
      excel('A4', '=HEX2BIN("FFFFFFFFFF")'),
      excel('B1', 'Description'),
      excel('B2', 'Converts hexadecimal F to binary, with 8 characters (4 zeros at beginning are "padding").'),
      excel('B3', 'Converts hexadecimal B7 to binary.'),
      excel('B4', 'Converts hexadecimal FFFFFFFFFF to binary.'),
      excel('C1', 'Result'),

      shouldBe('A2', valueS('00001111')),
      shouldBe('A3', valueS('10110111')),
      shouldBe('A4', valueS('1111111111')),

      exec(done)
    ]);
  });
  it('HEX2DEC', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=HEX2DEC("A5")'),
      excel('A3', '=HEX2DEC("FFFFFFFF5B")'),
      excel('A4', '=HEX2DEC("3DA408B9")'),
      excel('B1', 'Description'),
      excel('B2', 'Converts hexadecimal A5 to decimal'),
      excel('B3', 'Converts hexadecimal FFFFFFFF5B to decimal'),
      excel('B4', 'Converts hexadecimal 3DA408B9 to decimal'),
      excel('C1', 'Result'),

      shouldBe('A2', valueI(165)),
      shouldBe('A3', valueI(-165)),
      shouldBe('A4', valueD(1.034E+09)),

      exec(done)
    ]);
  });
  it('HEX2OCT', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=HEX2OCT("F", 3)'),
      excel('A3', '=HEX2OCT("3B4E")'),
      excel('A4', '=HEX2OCT("FFFFFFFF00")'),
      excel('B1', 'Description'),
      excel('B2', 'Converts hexadecimal F to octal with 3 characters (the leading 0 is "padding").'),
      excel('B3', 'Converts hexadecimal 3B4E to octal'),
      excel('B4', 'Converts hexadecimal FFFFFFFF00 to octal'),
      excel('C1', 'Result'),

      shouldBe('A2', valueI(17)),
      shouldBe('A3', valueI(35516)),
      shouldBe('A4', valueI(7777777400)),

      exec(done)
    ]);
  });
  it('IMABS', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=IMABS("5+12i")'),
      excel('B1', 'Description'),
      excel('B2', 'Absolute value of 5+12i'),
      excel('C1', 'Result'),

      shouldBe('A2', valueI(13)),

      exec(done)
    ]);
  });
  it('IMAGINARY', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=IMAGINARY("3+4i")'),
      excel('A3', '=IMAGINARY("0-j")'),
      excel('A4', '=IMAGINARY(4)'),
      excel('B1', 'Description'),
      excel('B2', 'Imaginary coefficient of the complex number 3+4i'),
      excel('B3', 'Imaginary coefficient of the complex number 0-j'),
      excel('B4', 'Imaginary coefficient 4'),
      excel('C1', 'Result'),

      shouldBe('A2', valueI(4)),
      shouldBe('A3', valueI(-1)),
      shouldBe('A4', valueI(0)),

      exec(done)
    ]);
  });
  it('IMARGUMENT', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=IMARGUMENT("3+4i")'),
      excel('B1', 'Description'),
      excel('B2', 'Theta argument of 3+4i, in radians'),
      excel('C1', 'Result'),

      shouldBe('A2', valueD(0.92729522)),

      exec(done)
    ]);
  });
  it('IMCONJUGATE', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=IMCONJUGATE("3+4i")'),
      excel('B1', 'Description'),
      excel('B2', 'Complex conjugate of 3+4i'),
      excel('C1', 'Result'),

      shouldBe('A2', valueS('3-4i')),

      exec(done)
    ]);
  });
  it('IMCOS', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=IMCOS("1+i")'),
      excel('B1', 'Description'),
      excel('B2', 'Cosine of 1+i'),
      excel('C1', 'Result'),

      shouldBe('A2', valueS('0.833730025131149-0.988897705762865i')),

      exec(done)
    ]);
  });
  it('IMCOSH', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=IMCOSH("4+3i")'),
      excel('B1', 'Description'),
      excel('B2', 'Returns the  hyperbolic cosine of a complex number, 4+3i.'),
      excel('C1', 'Result'),

      shouldBe('A2', valueS('-27.0349456030742+3.85115333481178i')),

      exec(done)
    ]);
  });
  it('IMCOT', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=IMCOT("4+3i")'),
      excel('B1', 'Description'),
      excel('B2', 'Returns the cotangent of a complex number, 4+3i.'),
      excel('C1', 'Result'),

      shouldBe('A2', valueS('0.00490118239430447-0.999266927805902i')),

      exec(done)
    ]);
  });
  it('IMCSC', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=IMCSC("4+3i")'),
      excel('B1', 'Description'),
      excel('B2', 'Returns the cosecant of a complex number, 4+3i.'),
      excel('C1', 'Result'),

      shouldBe('A2', valueS('-0.0754898329158637+0.0648774713706355i')),

      exec(done)
    ]);
  });
  it('IMCSCH', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=IMCSCH("4+3i")'),
      excel('B1', 'Description'),
      excel('B2', 'Returns the hyperbolic cosecant of a complex number, 4+3i.'),
      excel('C1', 'Result'),

      shouldBe('A2', valueS('-0.036275889628626-0.0051744731840194i')),

      exec(done)
    ]);
  });
  it('IMDIV', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=IMDIV("-238+240i","10+24i")'),
      excel('B1', 'Description'),
      excel('B2', 'Quotient of the two complex numbers in the formula.'),
      excel('C1', 'Result'),

      shouldBe('A2', valueS('5+12i')),

      exec(done)
    ]);
  });
  it('IMEXP', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=IMEXP("1+i")'),
      excel('B1', 'Description'),
      excel('B2', 'Exponential of the complex number 1+i'),
      excel('C1', 'Result'),

      shouldBe('A2', valueS('1.46869393991589+2.28735528717884i')),

      exec(done)
    ]);
  });
  it('IMLN', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=IMLN("3+4i")'),
      excel('B1', 'Description'),
      excel('B2', 'Natural logarithm of 3+4i'),
      excel('C1', 'Result'),

      shouldBe('A2', valueS('1.6094379124341+0.927295218001612i')),

      exec(done)
    ]);
  });
  it('IMLOG10', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=IMLOG10("3+4i")'),
      excel('B1', 'Description'),
      excel('B2', 'Logarithm (base 10) of 3+4i'),
      excel('C1', 'Result'),

      shouldBe('A2', valueS('0.698970004336019+0.402719196273373i')),

      exec(done)
    ]);
  });
  it('IMLOG2', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=IMLOG2("3+4i")'),
      excel('B1', 'Description'),
      excel('B2', 'Base-2 logarithm of 3+4i'),
      excel('C1', 'Result'),

      shouldBe('A2', valueS('2.32192809488736+1.33780421245098i')),

      exec(done)
    ]);
  });
  it('IMPOWER', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=IMPOWER("2+3i", 3)'),
      excel('B1', 'Description'),
      excel('B2', '2+3i raised to the power of 3 (-46 + 9i)'),
      excel('C1', 'Result'),

      shouldBe('A2', valueS('-46+9.00000000000001i')),

      exec(done)
    ]);
  });
  it('IMPRODUCT', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=IMPRODUCT("3+4i","5-3i")'),
      excel('A3', '=IMPRODUCT("1+2i",30)'),
      excel('B1', 'Description'),
      excel('B2', 'Product of two complex numbers'),
      excel('B3', 'Product of a complex number and 30'),
      excel('C1', 'Result'),

      shouldBe('A2', valueS('27+11i')),
      shouldBe('A3', valueS('30+60i')),

      exec(done)
    ]);
  });
  it('IMREAL', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=IMREAL("6-9i")'),
      excel('B1', 'Description'),
      excel('B2', 'Real coefficient of 6-9i'),
      excel('C1', 'Result'),

      shouldBe('A2', valueI(6)),

      exec(done)
    ]);
  });
  it('IMSEC', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=IMSEC("4+3i")'),
      excel('B1', 'Description'),
      excel('B2', 'Returns the secant of a complex number, 4+3i.'),
      excel('C1', 'Result'),

      shouldBe('A2', valueS('-0.0652940278579471-0.0752249603027732i')),

      exec(done)
    ]);
  });
  it('IMSECH', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=IMSECH("4+3i")'),
      excel('B1', 'Description'),
      excel('B2', 'Returns the hyperbolic secant of a complex number, 4+3i.'),
      excel('C1', 'Result'),

      shouldBe('A2', valueS('-0.0362534969158689-0.00516434460775318i')),

      exec(done)
    ]);
  });
  it('IMSIN', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=IMSIN("4+3i")'),
      excel('B1', 'Description'),
      excel('B2', 'Returns the sine of a complex number, 4+3i.'),
      excel('C1', 'Result'),

      shouldBe('A2', valueS('-7.61923172032141-6.548120040911i')),

      exec(done)
    ]);
  });
  it('IMSINH', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=IMSINH("4+3i")'),
      excel('B1', 'Description'),
      excel('B2', 'Returns the hyperbolic sine of a complex number, 4+3i.'),
      excel('C1', 'Result'),

      shouldBe('A2', valueS('-7.61923172032141-6.548120040911i')),

      exec(done)
    ]);
  });
  it('IMSQRT', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=IMSQRT("1+i")'),
      excel('B1', 'Description'),
      excel('B2', 'Square root of 1+i'),
      excel('C1', 'Result'),

      shouldBe('A2', valueS('1.09868411346781+0.455089860562227i')),

      exec(done)
    ]);
  });
  it('IMSUB', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=IMSUB("13+4i","5+3i")'),
      excel('B1', 'Description'),
      excel('B2', 'Difference between the two complex numbers in the formula.'),
      excel('C1', 'Result'),

      shouldBe('A2', valueS('8+i')),

      exec(done)
    ]);
  });
  it('IMSUM', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=IMSUM("3+4i","5-3i")'),
      excel('B1', 'Description'),
      excel('B2', 'Sum of two complex numbers'),
      excel('C1', 'Result'),

      shouldBe('A2', valueS('8+i')),

      exec(done)
    ]);
  });
  it('IMTAN', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=IMTAN("4+3i")'),
      excel('B1', 'Description'),
      excel('B2', 'Returns the tangent of a complex number, 4+3i.'),
      excel('C1', 'Result'),

      shouldBe('A2', valueS('0.00490825806749606+1.00070953606723i')),

      exec(done)
    ]);
  });
  it('OCT2BIN', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=OCT2BIN(3, 3)'),
      excel('A3', '=OCT2BIN(7777777000)'),
      excel('B1', 'Description'),
      excel('B2', 'Converts octal 3 to binary with 3 characters'),
      excel('B3', 'Converts octal 7777777000 to binary'),
      excel('C1', 'Result'),

      shouldBe('A2', valueS('011')),
      shouldBe('A3', valueS('1000000000')),

      exec(done)
    ]);
  });
  it('OCT2DEC', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=OCT2DEC(54)'),
      excel('A3', '=OCT2DEC(7777777533)'),
      excel('B1', 'Description'),
      excel('B2', 'Converts octal 54 to decimal form.'),
      excel('B3', 'Converts octal 7777777533 to decimal form.'),
      excel('C1', 'Result'),

      shouldBe('A2', valueI(44)),
      shouldBe('A3', valueI(-165)),

      exec(done)
    ]);
  });
  it('OCT2HEX', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=OCT2HEX(100, 4)'),
      excel('A3', '=OCT2HEX(7777777533)'),
      excel('B1', 'Description'),
      excel('B2', 'Converts octal number 100 to hexadecimal form with 4 characters.'),
      excel('B3', 'Converts octal number 7777777533 to hexadecimal form.'),
      excel('C1', 'Result'),

      shouldBe('A2', valueI(40)),
      shouldBe('A3', valueS('FFFFFFFF5B')),

      exec(done)
    ]);
  });
  it('ACCRINT', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A10', '=ACCRINT(A2,A3,A4,A5,A6,A7,A8)'),
      excel('A11', '=ACCRINT(DATE(2008,3,5),A3,A4,A5,A6,A7,A8,FALSE)'),
      excel('A12', '=ACCRINT(DATE(2008, 4, 5), A3, A4, A5, A6, A7, A8, TRUE)'),
      excel('A2', '39508'),
      excel('A3', '39691'),
      excel('A4', '39569'),
      excel('A5', '0.1'),
      excel('A6', '1000'),
      excel('A7', '2'),
      excel('A8', '0'),
      excel('A9', 'Formula'),
      excel('B1', 'Description'),
      excel('B10', 'Accrued interest for a treasury bond with the terms above.'),
      excel('B11', 'Accrued interest with the terms above, except the issue date is March 5, 2008.'),
      excel('B12', 'Accrued interest with the terms above, except the issue date is April 5, 2008, and the accrued interest is calculated from the first_interest to settlement.'),
      excel('B2', 'Issue date'),
      excel('B3', 'First interest date'),
      excel('B4', 'Settlement date'),
      excel('B5', 'Coupon rate'),
      excel('B6', 'Par value'),
      excel('B7', 'Frequency is semiannual (see above)'),
      excel('B8', '30/360 basis (see above)'),
      excel('B9', 'Description'),
      excel('C9', 'Result'),

      shouldBe('A10', valueD(16.666667)),
      shouldBe('A11', valueD(15.555556)),
      shouldBe('A12', valueD(7.2222222)),

      exec(done)
    ]);
  });
  it('ACCRINTM', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A2', '39539'),
      excel('A3', '39614'),
      excel('A4', '0.1'),
      excel('A5', '1000'),
      excel('A6', '3'),
      excel('A7', 'Formula'),
      excel('A8', '=ACCRINTM(A2,A3,A4,A5,A6)'),
      excel('B1', 'Description'),
      excel('B2', 'Issue date'),
      excel('B3', 'Maturity date'),
      excel('B4', 'Percent coupon'),
      excel('B5', 'Par value'),
      excel('B6', 'Actual/365 basis (see above)'),
      excel('B7', 'Description'),
      excel('B8', 'The accrued interest for the terms above.'),
      excel('C7', 'Result'),

      shouldBe('A8', valueD(20.54794521)),

      exec(done)
    ]);
  });
  it('AMORDEGRC', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A10', '=AMORDEGRC(A2,A3,A4,A5,A6,A7,A8)'),
      excel('A2', '2400'),
      excel('A3', '39679'),
      excel('A4', '39813'),
      excel('A5', '300'),
      excel('A6', '1'),
      excel('A7', '0.15'),
      excel('A8', '1'),
      excel('A9', 'Formula'),
      excel('B1', 'Description'),
      excel('B10', 'First period depreciation'),
      excel('B2', 'Cost'),
      excel('B3', 'Date purchased'),
      excel('B4', 'End of the first period'),
      excel('B5', 'Salvage value'),
      excel('B6', 'Period'),
      excel('B7', 'Depreciation rate'),
      excel('B8', 'Actual basis (see above)'),
      excel('B9', 'Description'),
      excel('C9', 'Result'),

      shouldBe('A10', valueI(776)),

      exec(done)
    ]);
  });
  it('AMORLINC', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A10', '=AMORLINC(A2,A3,A4,A5,A6,A7,A7)'),
      excel('A2', '2400'),
      excel('A3', '39679'),
      excel('A4', '39813'),
      excel('A5', '300'),
      excel('A6', '1'),
      excel('A7', '0.15'),
      excel('A8', '1'),
      excel('A9', 'Formula'),
      excel('B1', 'Description'),
      excel('B10', 'First period depreciation'),
      excel('B2', 'Cost'),
      excel('B3', 'Date purchased'),
      excel('B4', 'End of the first period'),
      excel('B5', 'Salvage value'),
      excel('B6', 'Period'),
      excel('B7', 'Depreciation rate'),
      excel('B8', 'Actual basis (see above)'),
      excel('B9', 'Description'),
      excel('C9', 'Result'),

      shouldBe('A10', valueI(360)),

      exec(done)
    ]);
  });
  it('COUPDAYBS', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A2', '25-Jan-11'),
      excel('A3', '15-Nov-11'),
      excel('A4', '2'),
      excel('A5', '1'),
      excel('A6', 'Formula'),
      excel('A7', '=COUPDAYBS(A2,A3,A4,A5)'),
      excel('B1', 'Description'),
      excel('B2', 'Settlement date'),
      excel('B3', 'Maturity date'),
      excel('B4', 'Semiannual coupon (see above)'),
      excel('B5', 'Actual/actual basis (see above)'),
      excel('B6', 'Description'),
      excel('B7', 'The number of days from the beginning of the coupon period to the settlement date, for a bond with the above terms'),
      excel('C6', 'Result'),

      shouldBe('A7', valueI(71)),

      exec(done)
    ]);
  });
  it('COUPDAYS', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A2', '25-Jan-11'),
      excel('A3', '15-Nov-11'),
      excel('A4', '2'),
      excel('A5', '1'),
      excel('A6', 'Formula'),
      excel('A7', '=COUPDAYS(A2,A3,A4,A5)'),
      excel('B1', 'Description'),
      excel('B2', 'Settlement date'),
      excel('B3', 'Maturity date'),
      excel('B4', 'Semiannual coupon (see above)'),
      excel('B5', 'Actual/actual basis (see above)'),
      excel('B6', 'Description'),
      excel('B7', 'The number of days in the coupon period that contains the settlement date, for a bond with the above terms.'),
      excel('C6', 'Result'),

      shouldBe('A7', valueI(181)),

      exec(done)
    ]);
  });
  it('COUPDAYSNC', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A2', '25-Jan-11'),
      excel('A3', '15-Nov-11'),
      excel('A4', '2'),
      excel('A5', '1'),
      excel('A6', 'Formula'),
      excel('A7', '=COUPDAYSNC(A2,A3,A4,A5)'),
      excel('B1', 'Description'),
      excel('B2', 'Settlement date'),
      excel('B3', 'Maturity date'),
      excel('B4', 'Semiannual coupon (see above)'),
      excel('B5', 'Actual/actual basis (see above)'),
      excel('B6', 'Description'),
      excel('B7', 'The number of days from the settlement date to the next coupon date, for a bond with the above terms'),
      excel('C6', 'Result'),

      shouldBe('A7', valueI(110)),

      exec(done)
    ]);
  });
  it('COUPNCD', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A2', '25-Jan-11'),
      excel('A3', '15-Nov-11'),
      excel('A4', '2'),
      excel('A5', '1'),
      excel('A6', 'Formula'),
      excel('A7', '=COUPNCD(A2,A3,A4,A5)'),
      excel('B1', 'Description'),
      excel('B2', 'Settlement date'),
      excel('B3', 'Maturity date'),
      excel('B4', 'Semiannual coupon (see above)'),
      excel('B5', 'Actual/actual basis (see above)'),
      excel('B6', 'Description'),
      excel('B7', 'The next coupon date after the settlement date, for a bond with the above terms'),
      excel('C6', 'Result'),

      shouldBe('A7', valueS('15-May-11')),

      exec(done)
    ]);
  });
  it('COUPNUM', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A2', '25-Jan-07'),
      excel('A3', '15-Nov-08'),
      excel('A4', '2'),
      excel('A5', '1'),
      excel('A6', 'Formula'),
      excel('A7', '=COUPNUM(A2,A3,A4,A5)'),
      excel('B1', 'Description'),
      excel('B2', 'Settlement date'),
      excel('B3', 'Maturity date'),
      excel('B4', 'Semiannual coupon (see above)'),
      excel('B5', 'Actual/actual basis (see above)'),
      excel('B6', 'Description'),
      excel('B7', 'The number of coupon payments for a bond with the above terms'),
      excel('C6', 'Result'),

      shouldBe('A7', valueI(4)),

      exec(done)
    ]);
  });
  it('COUPPCD', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A2', '25-Jan-11'),
      excel('A3', '15-Nov-11'),
      excel('A4', '2'),
      excel('A5', '1'),
      excel('A6', 'Formula'),
      excel('A7', '=COUPPCD(A2,A3,A4,A5)'),
      excel('B1', 'Description'),
      excel('B2', 'Settlement date'),
      excel('B3', 'Maturity date'),
      excel('B4', 'Semiannual coupon (see above)'),
      excel('B5', 'Actual/actual basis (see above)'),
      excel('B6', 'Description'),
      excel('B7', 'The previous coupon date before the settlement date, for a bond with the above terms.'),
      excel('C6', 'Result'),

      shouldBe('A7', valueS('15-Nov-10')),

      exec(done)
    ]);
  });
  it('CUMIPMT', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A2', '0.09'),
      excel('A3', '30'),
      excel('A4', '125000'),
      excel('A5', 'Formula'),
      excel('A6', '=CUMIPMT(A2/12,A3*12,A4,13,24,0)'),
      excel('A7', '=CUMIPMT(A2/12,A3*12,A4,1,1,0)'),
      excel('B1', 'Description'),
      excel('B2', 'Annual interest rate'),
      excel('B3', 'Years of the loan'),
      excel('B4', 'Present value'),
      excel('B5', 'Description'),
      excel('B6', 'Total interest paid in the second year of payments, periods 13 through 24'),
      excel('B7', 'Interest paid in a single payment in the first month'),
      excel('C5', 'Result'),

      shouldBe('A6', valueD(-11135.23213)),
      shouldBe('A7', valueD(-937.5)),

      exec(done)
    ]);
  });
  it('CUMPRINC', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A2', '0.09'),
      excel('A3', '30'),
      excel('A4', '125000'),
      excel('A5', 'Formula'),
      excel('A6', '=CUMPRINC(A2/12,A3*12,A4,13,24,0)'),
      excel('A7', '=CUMPRINC(A2/12,A3*12,A4,1,1,0)'),
      excel('B1', 'Description'),
      excel('B2', 'Interest rate per annum'),
      excel('B3', 'Term in years'),
      excel('B4', 'Present value'),
      excel('B5', 'Description'),
      excel('B6', 'The total principal paid in the second year of payments, periods 13 through 24'),
      excel('B7', 'The principal paid in a single payment in the first month (-68.27827)'),
      excel('C5', 'Result'),

      shouldBe('A6', valueD(-934.1071234)),
      shouldBe('A7', valueD(-68.27827118)),

      exec(done)
    ]);
  });
  it('DB', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A10', '=DB(A2,A3,A4,5,7)'),
      excel('A11', '=DB(A2,A3,A4,6,7)'),
      excel('A12', '=DB(A2,A3,A4,7,7)'),
      excel('A2', '$1,000,000'),
      excel('A3', '$100,000'),
      excel('A4', '6'),
      excel('A5', 'Formula'),
      excel('A6', '=DB(A2,A3,A4,1,7)'),
      excel('A7', '=DB(A2,A3,A4,2,7)'),
      excel('A8', '=DB(A2,A3,A4,3,7)'),
      excel('A9', '=DB(A2,A3,A4,4,7)'),
      excel('B1', 'Description'),
      excel('B10', 'Depreciation in fifth year'),
      excel('B11', 'Depreciation in sixth year'),
      excel('B12', 'Depreciation in seventh year, with only 5 months calculated'),
      excel('B2', 'Initial cost'),
      excel('B3', 'Salvage value'),
      excel('B4', 'Lifetime in years'),
      excel('B5', 'Description'),
      excel('B6', 'Depreciation in first year, with only 7 months calculated'),
      excel('B7', 'Depreciation in second year'),
      excel('B8', 'Depreciation in third year'),
      excel('B9', 'Depreciation in fourth year'),
      excel('C5', 'Result'),

      shouldBe('A10', valueS('$81,999.64')),
      shouldBe('A11', valueS('$55,841.76')),
      shouldBe('A12', valueS('$15,845.10')),
      shouldBe('A6', valueS('$186,083.33')),
      shouldBe('A7', valueS('$259,639.42')),
      shouldBe('A8', valueS('$176,814.44')),
      shouldBe('A9', valueS('$120,410.64')),

      exec(done)
    ]);
  });
  it('DDB', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A10', '=DDB(A2,A3,A4,10)'),
      excel('A2', '$2,400'),
      excel('A3', '$300'),
      excel('A4', '10'),
      excel('A5', 'Formula'),
      excel('A6', '=DDB(A2,A3,A4*365,1)'),
      excel('A7', '=DDB(A2,A3,A4*12,1,2)'),
      excel('A8', '=DDB(A2,A3,A4,1,2)'),
      excel('A9', '=DDB(A2,A3,A4,2,1.5)'),
      excel('B1', 'Description'),
      excel('B10', 'Tenth year\'s depreciation. Default factor is 2.'),
      excel('B2', 'Initial cost'),
      excel('B3', 'Salvage value'),
      excel('B4', 'Lifetime in years'),
      excel('B5', 'Description'),
      excel('B6', 'First day\'s depreciation, using double-declining balance method. Default factor is 2.'),
      excel('B7', 'First month\'s depreciation.'),
      excel('B8', 'First year\'s depreciation.'),
      excel('B9', 'Second year\'s depreciation using a factor of 1.5 instead of the double-declining balance method.'),
      excel('C5', 'Result'),

      shouldBe('A10', valueS('$22.12')),
      shouldBe('A6', valueS('$1.32')),
      shouldBe('A7', valueS('$40.00')),
      shouldBe('A8', valueS('$480.00')),
      shouldBe('A9', valueS('$306.00')),

      exec(done)
    ]);
  });
  it('DISC', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A2', '39107'),
      excel('A3', '39248'),
      excel('A4', '97.975'),
      excel('A5', '100'),
      excel('A6', '1'),
      excel('A7', 'Formula'),
      excel('A8', '=DISC(A2,A3,A4,A5,A6)'),
      excel('B1', 'Description'),
      excel('B2', 'Settlement date'),
      excel('B3', 'Maturity date'),
      excel('B4', 'Price'),
      excel('B5', 'Redemption value'),
      excel('B6', 'Actual/actual basis (see above)'),
      excel('B7', 'Description'),
      excel('B8', 'The bond discount rate, for a bond with the above terms'),
      excel('C7', 'Result'),

      shouldBe('A8', valueD(0.0524202)),

      exec(done)
    ]);
  });
  it('DOLLARDE', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=DOLLARDE(1.02,16)'),
      excel('A3', '=DOLLARDE(1.1,32)'),
      excel('B1', 'Description'),
      excel('B2', 'Converts 1.02, read as 1 and 2/16, to a decimal number (1.125). Because the fraction value is 16, the price has a precision of 1/16 of a dollar.'),
      excel('B3', 'Converts 1.1, read as 1 and 10/32, to a decimal number (1.3125). Because the fraction value is 32, the price has a precision of 1/32 of a dollar.'),
      excel('C1', 'Result'),

      shouldBe('A2', valueD(1.125)),
      shouldBe('A3', valueD(1.3125)),

      exec(done)
    ]);
  });
  it('DOLLARFR', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=DOLLARFR(1.125,16)'),
      excel('A3', '=DOLLARFR(1.125,32)'),
      excel('B1', 'Description'),
      excel('B2', 'Converts the decimal number 1.125 to a number read as 1 and 2/16.'),
      excel('B3', 'Converts the decimal number 1.125 to a number read as 1 and 4/32.'),
      excel('C1', 'Result'),

      shouldBe('A2', valueD(1.02)),
      shouldBe('A3', valueD(1.04)),

      exec(done)
    ]);
  });
  it('DURATION', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A2', '39448'),
      excel('A3', '42370'),
      excel('A4', '0.08'),
      excel('A5', '0.09'),
      excel('A6', '2'),
      excel('A7', '1'),
      excel('A8', 'Formula'),
      excel('A9', '=DURATION(A2,A3,A4,A5,A6,A7)'),
      excel('B1', 'Description'),
      excel('B2', 'Settlement date'),
      excel('B3', 'Maturity date'),
      excel('B4', 'Percent coupon'),
      excel('B5', 'Percent yield'),
      excel('B6', 'Frequency is semiannual (see above)'),
      excel('B7', 'Actual/actual basis (see above)'),
      excel('B8', 'Description'),
      excel('B9', 'The duration, for the bond with the terms above'),
      excel('C8', 'Result'),

      shouldBe('A9', valueD(5.99377496)),

      exec(done)
    ]);
  });
  it('EFFECT', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A2', '0.0525'),
      excel('A3', '4'),
      excel('A4', 'Formula'),
      excel('A5', '=EFFECT(A2,A3)'),
      excel('B1', 'Description'),
      excel('B2', 'Nominal interest rate'),
      excel('B3', 'Number of compounding periods per year'),
      excel('B4', 'Description'),
      excel('B5', 'Effective interest rate with the terms above'),
      excel('C4', 'Result'),

      shouldBe('A5', valueD(0.0535427)),

      exec(done)
    ]);
  });
  it('FV', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A2', '0.06'),
      excel('A3', '10'),
      excel('A4', '-200'),
      excel('A5', '-500'),
      excel('A6', '1'),
      excel('A7', 'Formula'),
      excel('A8', '=FV(A2/12, A3, A4, A5, A6)'),
      excel('B1', 'Description'),
      excel('B2', 'Annual interest rate'),
      excel('B3', 'Number of payments'),
      excel('B4', 'Amount of the payment'),
      excel('B5', 'Present value'),
      excel('B6', 'Payment is due at the beginning of the period (0 indicates payment is due at end of period)'),
      excel('B7', 'Description'),
      excel('B8', 'Future value of an investment using the terms in A2:A5.'),
      excel('C7', 'Result'),

      shouldBe('A8', valueS('$2,581.40')),

      exec(done)
    ]);
  });
  it('FV', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A2', '0.12'),
      excel('A3', '12'),
      excel('A4', '-1000'),
      excel('A5', 'Formula'),
      excel('A6', '=FV(A2/12, A3, A4)'),
      excel('B1', 'Description'),
      excel('B2', 'Annual interest rate'),
      excel('B3', 'Number of payments'),
      excel('B4', 'Amount of the payment'),
      excel('B5', 'Description'),
      excel('B6', 'Future value of an investment using the terms in A2:A4.'),
      excel('C5', 'Result'),

      shouldBe('A6', valueS('$12,682.50')),

      exec(done)
    ]);
  });
  it('FV', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A2', '0.11'),
      excel('A3', '35'),
      excel('A4', '-2000'),
      excel('A5', '1'),
      excel('A6', 'Formula'),
      excel('A7', '=FV(A2/12, A3, A4,, A5)'),
      excel('B1', 'Description'),
      excel('B2', 'Annual interest rate'),
      excel('B3', 'Number of payments'),
      excel('B4', 'Amount of the payment'),
      excel('B5', 'Payment is due at the beginning of the year (0 indicates end of year)'),
      excel('B6', 'Description'),
      excel('B7', 'Future value of an investment with the terms in cells A2:A4'),
      excel('C6', 'Result'),

      shouldBe('A7', valueS('$82,846.25')),

      exec(done)
    ]);
  });
  it('FV', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A2', '0.06'),
      excel('A3', '12'),
      excel('A4', '-100'),
      excel('A5', '-1000'),
      excel('A6', '1'),
      excel('A7', 'Formula'),
      excel('A8', '=FV(A2/12, A3, A4, A5, A6)'),
      excel('B1', 'Description'),
      excel('B2', 'Annual interest rate'),
      excel('B3', 'Number of payments'),
      excel('B4', 'Amount of the payment'),
      excel('B5', 'Present value'),
      excel('B6', 'Payment is due at the beginning of the year (0 indicates end of year)'),
      excel('B7', 'Description'),
      excel('B8', 'Future value of an investment using the terms in A2:A5.'),
      excel('C7', 'Result'),

      shouldBe('A8', valueS('$2,301.40')),

      exec(done)
    ]);
  });
  it('FVSCHEDULE', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=FVSCHEDULE(1,{0.09,0.11,0.1})'),
      excel('B1', 'Description'),
      excel('B2', 'Future value of 1 with compound annual interest rates of 9%, 11%, and 10%.'),
      excel('C1', 'Result'),

      shouldBe('A2', valueD(1.3309)),

      exec(done)
    ]);
  });
  it('INTRATE', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A2', '2/15/2008'),
      excel('A3', '5/15/2008'),
      excel('A4', '$1,000,000'),
      excel('A5', '$1,014,420'),
      excel('A6', '2'),
      excel('A7', 'Formula'),
      excel('A8', '=INTRATE(A2,A3,A4,A5,A6)'),
      excel('B1', 'Description'),
      excel('B2', 'Settlement date'),
      excel('B3', 'Maturity date'),
      excel('B4', 'Investment'),
      excel('B5', 'Redemption value'),
      excel('B6', 'Actual/360 basis'),
      excel('B7', 'Description (Result)'),
      excel('B8', 'Discount rate, for the terms of the bond (0.05768 or 5.77%)'),
      excel('C7', 'Result'),

      shouldBe('A8', valueS('5.77%')),

      exec(done)
    ]);
  });
  it('IPMT', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A2', '10.00%'),
      excel('A3', '1'),
      excel('A4', '3'),
      excel('A5', '$8,000'),
      excel('A6', 'Formula'),
      excel('A7', '=IPMT(A2/12, A3, A4*12, A5)'),
      excel('A8', '=IPMT(A2, 3, A4, A5)'),
      excel('B1', 'Description'),
      excel('B2', 'Annual interest'),
      excel('B3', 'Period for which you want to find the interest paid.'),
      excel('B4', 'Years of loan'),
      excel('B5', 'Present value of loan'),
      excel('B6', 'Description'),
      excel('B7', 'Interest due in the first month for a loan with the terms in A2:A5.'),
      excel('B8', 'Interest due in the last year for a loan with the same terms, where payments are made yearly.'),
      excel('C6', 'Live Result'),

      shouldBe('A7', valueS('($66.67)')),
      shouldBe('A8', valueS('($292.45)')),

      exec(done)
    ]);
  });
  it('IRR', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A10', '=IRR(A2:A7)'),
      excel('A11', '=IRR(A2:A4,-10%)'),
      excel('A2', '-$70,000'),
      excel('A3', '$12,000'),
      excel('A4', '$15,000'),
      excel('A5', '$18,000'),
      excel('A6', '$21,000'),
      excel('A7', '$26,000'),
      excel('A8', 'Formula'),
      excel('A9', '=IRR(A2:A6)'),
      excel('B1', 'Description'),
      excel('B10', 'Internal rate of return after five years'),
      excel('B11', 'To calculate the internal rate of return after two years, you need to include a guess (in this example, -10%).'),
      excel('B2', 'Initial cost of a business'),
      excel('B3', 'Net income for the first year'),
      excel('B4', 'Net income for the second year'),
      excel('B5', 'Net income for the third year'),
      excel('B6', 'Net income for the fourth year'),
      excel('B7', 'Net income for the fifth year'),
      excel('B8', 'Description'),
      excel('B9', 'Investment\'s internal rate of return after four years'),
      excel('C8', 'Result'),

      shouldBe('A10', valueS('8.7%')),
      shouldBe('A11', valueS('-44.4%')),
      shouldBe('A9', valueS('-2.1%')),

      exec(done)
    ]);
  });
  it('ISPMT', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A2', '0.1'),
      excel('A3', '1'),
      excel('A4', '3'),
      excel('A5', '8000000'),
      excel('A6', 'Formula'),
      excel('A7', '=ISPMT(A2/12,A3,A4*12,A5)'),
      excel('A8', '=ISPMT(A2,1,A4,A5)'),
      excel('B1', 'Description'),
      excel('B2', 'Annual interest rate'),
      excel('B3', 'Period'),
      excel('B4', 'Number of years in the investment'),
      excel('B5', 'Amount of loan'),
      excel('B6', 'Description'),
      excel('B7', 'Interest paid for the first monthly payment of a loan with the above terms'),
      excel('B8', 'Interest paid in the first year of a loan with the above terms'),
      excel('C6', 'Result'),

      shouldBe('A7', valueD(-64814.8148)),
      shouldBe('A8', valueD(-533333.333)),

      exec(done)
    ]);
  });
  it('MDURATION', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A2', '1/1/2008'),
      excel('A3', '1/1/2016'),
      excel('A4', '8%'),
      excel('A5', '9%'),
      excel('A6', '2'),
      excel('A7', '1'),
      excel('A8', 'Formula'),
      excel('A9', '=MDURATION(A2,A3,A4,A5,A6,A7)'),
      excel('B1', 'Description'),
      excel('B2', 'Settlement date'),
      excel('B3', 'Maturity date'),
      excel('B4', 'Percent coupon'),
      excel('B5', 'Percent yield'),
      excel('B6', 'Frequency is semiannual (see above)'),
      excel('B7', 'Actual/actual basis (see above)'),
      excel('B8', 'Description'),
      excel('B9', 'The modified duration, for the bond with the terms specified in A2:A5.'),
      excel('C8', 'Result'),

      shouldBe('A9', valueD(5.736)),

      exec(done)
    ]);
  });
  it('MIRR', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A10', 'Formula'),
      excel('A11', '=MIRR(A2:A7, A8, A9)'),
      excel('A12', '=MIRR(A2:A5, A8, A9)'),
      excel('A13', '=MIRR(A2:A7, A8, 14%)'),
      excel('A2', '-120000'),
      excel('A3', '39000'),
      excel('A4', '30000'),
      excel('A5', '21000'),
      excel('A6', '37000'),
      excel('A7', '46000'),
      excel('A8', '0.1'),
      excel('A9', '0.12'),
      excel('B1', 'Description'),
      excel('B10', 'Description'),
      excel('B11', 'Investment\'s modified rate of return after five years'),
      excel('B12', 'Modified rate of return after three years'),
      excel('B13', 'Five-year modified rate of return based on a reinvest_rate of 14 percent'),
      excel('B2', 'Initial cost'),
      excel('B3', 'Return first year'),
      excel('B4', 'Return second year'),
      excel('B5', 'Return third year'),
      excel('B6', 'Return fourth year'),
      excel('B7', 'Return fifth year'),
      excel('B8', 'Annual interest rate for the 120,000 loan'),
      excel('B9', 'Annual interest rate for the reinvested profits'),
      excel('C10', 'Result'),

      shouldBe('A11', valueS('13%')),
      shouldBe('A12', valueS('-5%')),
      shouldBe('A13', valueS('13%')),

      exec(done)
    ]);
  });
  it('NOMINAL', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A2', '0.053543'),
      excel('A3', '4'),
      excel('A4', 'Formula'),
      excel('A5', '=NOMINAL(A2,A3)'),
      excel('B1', 'Description'),
      excel('B2', 'Effective interest rate'),
      excel('B3', 'Number of compounding periods per year'),
      excel('B4', 'Description'),
      excel('B5', 'Nominal interest rate with the terms above'),
      excel('C4', 'Result'),

      shouldBe('A5', valueD(0.05250032)),

      exec(done)
    ]);
  });
  it('NPER', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A10', '=NPER(A2/12, A3, A4)'),
      excel('A2', '0.12'),
      excel('A3', '-100'),
      excel('A4', '-1000'),
      excel('A5', '10000'),
      excel('A6', '1'),
      excel('A7', 'Formula'),
      excel('A8', '=NPER(A2/12, A3, A4, A5, 1)'),
      excel('A9', '=NPER(A2/12, A3, A4, A5)'),
      excel('B1', 'Description'),
      excel('B10', 'Periods for the investment with the above terms, except with a future value of 0'),
      excel('B2', 'Annual interest rate'),
      excel('B3', 'Payment made each period'),
      excel('B4', 'Present value'),
      excel('B5', 'Future value'),
      excel('B6', 'Payment is due at the beginning of the period (see above)'),
      excel('B7', 'Description'),
      excel('B8', 'Periods for the investment with the above terms'),
      excel('B9', 'Periods for the investment with the above terms, except payments are made at the beginning of the period'),
      excel('C7', 'Live Result'),

      shouldBe('A10', valueD(-9.57859404)),
      shouldBe('A8', valueD(59.6738657)),
      shouldBe('A9', valueD(60.0821229)),

      exec(done)
    ]);
  });
  it('NPV', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A2', '0.1'),
      excel('A3', '-10000'),
      excel('A4', '3000'),
      excel('A5', '4200'),
      excel('A6', '6800'),
      excel('A7', 'Formula'),
      excel('A8', '=NPV(A2, A3, A4, A5, A6)'),
      excel('B1', 'Description'),
      excel('B2', 'Annual discount rate'),
      excel('B3', 'Initial cost of investment one year from today'),
      excel('B4', 'Return from first year'),
      excel('B5', 'Return from second year'),
      excel('B6', 'Return from third year'),
      excel('B7', 'Description'),
      excel('B8', 'Net present value of this investment'),
      excel('C7', 'Result'),

      shouldBe('A8', valueS('$1,188.44')),

      exec(done)
    ]);
  });
  it('NPV', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A10', '=NPV(A2, A4:A8)+A3'),
      excel('A11', '=NPV(A2, A4:A8, -9000)+A3'),
      excel('A2', '0.08'),
      excel('A3', '-40000'),
      excel('A4', '8000'),
      excel('A5', '9200'),
      excel('A6', '10000'),
      excel('A7', '12000'),
      excel('A8', '14500'),
      excel('A9', 'Formula'),
      excel('B1', 'Description'),
      excel('B10', 'Net present value of this investment'),
      excel('B11', 'Net present value of this investment, with a loss in the sixth year of 9000'),
      excel('B2', 'Annual discount rate. This might represent the rate of inflation or the interest rate of a competing investment.'),
      excel('B3', 'Initial cost of investment'),
      excel('B4', 'Return from first year'),
      excel('B5', 'Return from second year'),
      excel('B6', 'Return from third year'),
      excel('B7', 'Return from fourth year'),
      excel('B8', 'Return from fifth year'),
      excel('B9', 'Description'),
      excel('C9', 'Result'),

      shouldBe('A10', valueS('$1,922.06')),
      shouldBe('A11', valueS('($3,749.47)')),

      exec(done)
    ]);
  });
  it('ODDFPRICE', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A10', '1'),
      excel('A11', 'Formula'),
      excel('A12', '=ODDFPRICE(A2, A3, A4, A5, A6, A7, A8, A9, A10)'),
      excel('A2', '11/11/2008'),
      excel('A3', '3/1/2021'),
      excel('A4', '10/15/2008'),
      excel('A5', '3/1/2009'),
      excel('A6', '7.85%'),
      excel('A7', '6.25%'),
      excel('A8', '$100.00'),
      excel('A9', '2'),
      excel('B1', 'Argument description'),
      excel('B10', 'Actual/actual basis'),
      excel('B11', 'Description'),
      excel('B12', 'The price per $100 face value of a security having an odd (short or long) first period, for the bond using the terms in cells A2:A10 as arguments for the function.'),
      excel('B2', 'Settlement date'),
      excel('B3', 'Maturity date'),
      excel('B4', 'Issue date'),
      excel('B5', 'First coupon date'),
      excel('B6', 'Percent coupon'),
      excel('B7', 'Percent yield'),
      excel('B8', 'Redemptive value'),
      excel('B9', 'Frequency is semiannual'),
      excel('C11', 'Result'),

      shouldBe('A12', valueS('$  113.60')),

      exec(done)
    ]);
  });
  it('ODDFYIELD', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A10', '0'),
      excel('A11', 'Formula'),
      excel('A12', '=ODDFYIELD(A2, A3, A4, A5, A6, A7, A8, A9, A10)'),
      excel('A2', 'November 11, 2008'),
      excel('A3', 'March 1, 2021'),
      excel('A4', 'October 15, 2008'),
      excel('A5', 'March 1, 2009'),
      excel('A6', '5.75%'),
      excel('A7', '84.50'),
      excel('A8', '100'),
      excel('A9', '2'),
      excel('B1', 'Argument description'),
      excel('B10', '30/360 basis'),
      excel('B11', 'Description'),
      excel('B12', 'The yield of a security that has an odd (short or long) first period, for the bond using the terms in cells A2:A10 as function arguments. Result is (0.0772, or 7.72%).'),
      excel('B2', 'Settlement date'),
      excel('B3', 'Maturity date'),
      excel('B4', 'Issue date'),
      excel('B5', 'First coupon date'),
      excel('B6', 'Percent coupon'),
      excel('B7', 'Price'),
      excel('B8', 'Redemptive value'),
      excel('B9', 'Frequency is semiannual'),
      excel('C11', 'Result'),

      shouldBe('A12', valueS('7.72%')),

      exec(done)
    ]);
  });
  it('ODDLPRICE', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A10', 'Formula'),
      excel('A11', '=ODDLPRICE(A2, A3, A4, A5, A6, A7, A8, A9)'),
      excel('A2', 'February 7, 2008'),
      excel('A3', 'June 15, 2008'),
      excel('A4', 'October 15, 2007'),
      excel('A5', '3.75%'),
      excel('A6', '4.05%'),
      excel('A7', '$100'),
      excel('A8', '2'),
      excel('A9', '0'),
      excel('B1', 'Argument description'),
      excel('B10', 'Description'),
      excel('B11', 'The price per $100 of a security having an odd (short or long) last coupon period, for a bond using the terms in cells A2:A10 as function arguments.'),
      excel('B2', 'Settlement date'),
      excel('B3', 'Maturity date'),
      excel('B4', 'Last interest date'),
      excel('B5', 'Percent coupon'),
      excel('B6', 'Percent yield'),
      excel('B7', 'Redemptive value'),
      excel('B8', 'Frequency is semiannual'),
      excel('B9', '30/360 basis'),
      excel('C10', 'Result'),

      shouldBe('A11', valueS('$99.88')),

      exec(done)
    ]);
  });
  it('ODDLYIELD', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A10', 'Formula'),
      excel('A11', '=ODDLYIELD(A2, A3, A4, A5, A6, A7, A8, A9)'),
      excel('A2', '4/20/2008'),
      excel('A3', '6/15/2008'),
      excel('A4', '12/24/2007'),
      excel('A5', '3.75%'),
      excel('A6', '$99.875'),
      excel('A7', '$100'),
      excel('A8', '2'),
      excel('A9', '0'),
      excel('B1', 'Argument description'),
      excel('B10', 'Description'),
      excel('B11', 'The yield of a security that has an odd (short or long) last period, for the bond using the terms in cells A2:A10 as function arguments. Result is 0.04519, or 4.52%.'),
      excel('B2', 'Settlement date'),
      excel('B3', 'Maturity date'),
      excel('B4', 'Last interest date'),
      excel('B5', 'Percent coupon'),
      excel('B6', 'Price'),
      excel('B7', 'Redemption value'),
      excel('B8', 'Frequency is semiannual'),
      excel('B9', '30/360 basis'),
      excel('C10', 'Result'),

      shouldBe('A11', valueS('4.52%')),

      exec(done)
    ]);
  });
  it('PDURATION', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=PDURATION(2.5%,2000,2200)'),
      excel('A3', '=PDURATION(0.025/12,1000,1200)'),
      excel('B1', 'Description'),
      excel('B2', 'The number of years required for an investment of $2,000, earning 2.5% annually, to reach $2,200 (3.86 years).'),
      excel('B3', 'The number of months required for an investment of $1,000, earning 2.5% annually, to reach $1,200 (87.6 months).'),
      excel('C1', 'Result'),

      shouldBe('A2', valueD(3.86)),
      shouldBe('A3', valueD(87.6)),

      exec(done)
    ]);
  });
  it('PMT', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A11', 'Data'),
      excel('A12', '6%'),
      excel('A13', '18'),
      excel('A14', '$50,000'),
      excel('A16', 'Formula'),
      excel('A17', '=PMT(A12/12,A13*12, 0,A14)'),
      excel('A2', '8%'),
      excel('A3', '10'),
      excel('A4', '$10,000'),
      excel('A6', 'Formula'),
      excel('A7', '=PMT(A2/12,A3,A4)'),
      excel('A8', '=PMT(A2/12,A3,A4)'),
      excel('B1', 'Description'),
      excel('B11', 'Description'),
      excel('B12', 'Annual interest rate'),
      excel('B13', 'Number of months of payments'),
      excel('B14', 'Amount of loan'),
      excel('B16', 'Description'),
      excel('B17', 'Amount to save each month to have $50,000 at the end of 18 years.'),
      excel('B2', 'Annual interest rate'),
      excel('B3', 'Number of months of payments'),
      excel('B4', 'Amount of loan'),
      excel('B6', 'Description'),
      excel('B7', 'Monthly payment for a loan with terms specified as arguments in A2:A4.'),
      excel('B8', 'Monthly payment for a loan with with terms specified as arguments in A2:A4, except payments are due at the beginning of the period.'),
      excel('C16', 'Live Result'),
      excel('C6', 'Result'),

      shouldBe('A17', valueS('($129.08)')),
      shouldBe('A7', valueS('($1,037.03)')),
      shouldBe('A8', valueS('($1,030.16)')),

      exec(done)
    ]);
  });
  it('PPMT', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A10', 'Data'),
      excel('A11', '8%'),
      excel('A12', '10'),
      excel('A13', '$200,000.00'),
      excel('A14', 'Formula'),
      excel('A15', '=PPMT(A11, A12, 10, A13)'),
      excel('A2', '10%'),
      excel('A3', '2'),
      excel('A4', '$2,000.00'),
      excel('A5', 'Formula'),
      excel('A6', '=PPMT(A2/12, 1, A3*12, A4)'),
      excel('B1', 'Argument description'),
      excel('B10', 'Argument description'),
      excel('B11', 'Annual interest rate'),
      excel('B12', 'Number of years for the loan'),
      excel('B13', 'Amount of loan'),
      excel('B14', 'Description (Result)'),
      excel('B15', 'Principal payment for year 10 of the loan'),
      excel('B2', 'Annual interest rate'),
      excel('B3', 'Number of years for the loan'),
      excel('B4', 'Amount of loan'),
      excel('B5', 'Description'),
      excel('B6', 'Principal payment for month 1 of the loan'),
      excel('C14', 'Live Result'),
      excel('C5', 'Result'),

      shouldBe('A15', valueS('($27,598.05)')),
      shouldBe('A6', valueS('($75.62)')),

      exec(done)
    ]);
  });
  it('PRICE', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A10', '=PRICE(A2,A3,A4,A5,A6,A7,A8)'),
      excel('A2', '2/15/2008'),
      excel('A3', '11/15/2017'),
      excel('A4', '5.75%'),
      excel('A5', '6.50%'),
      excel('A6', '$100'),
      excel('A7', '2'),
      excel('A8', '0'),
      excel('A9', 'Formula'),
      excel('B1', 'Argument description'),
      excel('B10', 'The bond price, for the bond with the arguments specified in cells A2:A8.'),
      excel('B2', 'Settlement date'),
      excel('B3', 'Maturity date'),
      excel('B4', 'Percent semiannual coupon'),
      excel('B5', 'Percent yield'),
      excel('B6', 'Redemption value'),
      excel('B7', 'Frequency is semiannual'),
      excel('B8', '30/360 basis'),
      excel('B9', 'Description'),
      excel('C9', 'Result'),

      shouldBe('A10', valueS('$94.63')),

      exec(done)
    ]);
  });
  it('PRICEDISC', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A2', '2/16/2008'),
      excel('A3', '3/1/2008'),
      excel('A4', '5.25%'),
      excel('A5', '$100'),
      excel('A6', '2'),
      excel('A7', 'Formula'),
      excel('A8', '=PRICEDISC(A2,A3,A4,A5,A6)'),
      excel('B1', 'Argument description'),
      excel('B2', 'Settlement date'),
      excel('B3', 'Maturity date'),
      excel('B4', 'Percent discount rate'),
      excel('B5', 'Redemption value'),
      excel('B6', 'Actual/360 basis'),
      excel('B7', 'Description'),
      excel('B8', 'The bond price, for the bond with the arguments specified in cells A2:A6.'),
      excel('C7', 'Result'),

      shouldBe('A8', valueS('$99.80')),

      exec(done)
    ]);
  });
  it('PRICEMAT', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A2', '2/15/2008'),
      excel('A3', '4/13/2008'),
      excel('A4', '11/11/2007'),
      excel('A5', '6.10%'),
      excel('A6', '6.10%'),
      excel('A7', '0'),
      excel('A8', 'Formula'),
      excel('A9', '=PRICEMAT(A2,A3,A4,A5,A6,A7)'),
      excel('B1', 'Description'),
      excel('B2', 'Settlement date'),
      excel('B3', 'Maturity date'),
      excel('B4', 'Issue date'),
      excel('B5', 'Percent semiannual coupon'),
      excel('B6', 'Percent yield'),
      excel('B7', '30/360 basis'),
      excel('B8', 'Description'),
      excel('B9', 'The price for the bond, using the arguments specified in cells A2:A7.'),
      excel('C8', 'Result'),

      shouldBe('A9', valueS('$99.98')),

      exec(done)
    ]);
  });
  it('PV', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A2', '$500.000'),
      excel('A3', '8%'),
      excel('A4', '20'),
      excel('A5', 'Formula'),
      excel('A6', '=PV(A3/12, 12*A4, A2, , 0)'),
      excel('B1', 'Description'),
      excel('B2', 'Money paid out of an insurance annuity at the end of every month.'),
      excel('B3', 'Interest rate earned on the money paid out.'),
      excel('B4', 'Years the money will be paid out.'),
      excel('B5', 'Description'),
      excel('B6', 'Present value of an annuity with the terms in A2:A4.'),
      excel('C5', 'Result'),

      shouldBe('A6', valueS('($59,777.15)')),

      exec(done)
    ]);
  });
  it('RATE', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A2', '4'),
      excel('A3', '-200'),
      excel('A4', '8000'),
      excel('A5', 'Formula'),
      excel('A6', '=RATE(A2*12, A3, A4)'),
      excel('A7', '=RATE(A2*12, A3, A4)*12'),
      excel('B1', 'Description'),
      excel('B2', 'Years of the loan'),
      excel('B3', 'Monthly payment'),
      excel('B4', 'Amount of the loan'),
      excel('B5', 'Description'),
      excel('B6', 'Monthly rate of the loan with the terms entered as arguments in A2:A4.'),
      excel('B7', 'Annual rate of the loan with the same terms.'),
      excel('C5', 'Result'),

      shouldBe('A6', valueS('1%')),
      shouldBe('A7', valueS('9.24%')),

      exec(done)
    ]);
  });
  it('RECEIVED', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A2', '15-Feb-08'),
      excel('A3', '15-May-08'),
      excel('A4', '$                      1,000,000.00'),
      excel('A5', '5.75%'),
      excel('A6', '2'),
      excel('A7', 'Formula'),
      excel('A8', '=RECEIVED(A2,A3,A4,A5,A6)'),
      excel('B1', 'Description'),
      excel('B2', 'Settlement (issue) date'),
      excel('B3', 'Maturity date'),
      excel('B4', 'Investment'),
      excel('B5', 'Percent discount rate'),
      excel('B6', 'Actual/360 basis'),
      excel('B7', 'Description'),
      excel('B8', 'The total amount to be received at maturity, for the bond with the terms in A2:A6.'),
      excel('C7', 'Result'),

      shouldBe('A8', valueS('$ 1,014,584.65')),

      exec(done)
    ]);
  });
  it('RRI', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=RRI(96,10000,11000)'),
      excel('B1', 'Description'),
      excel('B2', 'Returns an equivalent interest rate for the growth of an investment of $10,000 with a future value of $11,000, for 8 years (0.012, or 1.2%).'),
      excel('C1', 'Result'),

      shouldBe('A2', valueD(0.0009933)),

      exec(done)
    ]);
  });
  it('SLN', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A2', '$30,000'),
      excel('A3', '$7,500'),
      excel('A4', '10'),
      excel('A5', 'Formula'),
      excel('A6', '=SLN(A2, A3, A4)'),
      excel('B1', 'Description'),
      excel('B2', 'Cost'),
      excel('B3', 'Salvage value'),
      excel('B4', 'Years of useful life'),
      excel('B5', 'Description'),
      excel('B6', 'The depreciation allowance for each year.'),
      excel('C5', 'Result'),

      shouldBe('A6', valueS('$2,250')),

      exec(done)
    ]);
  });
  it('SYD', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A2', '$            30,000.00'),
      excel('A3', '$              7,500.00'),
      excel('A4', '10'),
      excel('A5', 'Formula'),
      excel('A6', '=SYD(A2,A3,A4,1)'),
      excel('A7', '=SYD(A2,A3,A4,10)'),
      excel('B1', 'Description'),
      excel('B2', 'Initial cost'),
      excel('B3', 'Salvage value'),
      excel('B4', 'Lifespan in years'),
      excel('B5', 'Description (Result)'),
      excel('B6', 'Yearly depreciation allowance for the first year (4,090.91)'),
      excel('B7', 'Yearly depreciation allowance for the tenth year (409.09)'),
      excel('C5', 'Result'),

      shouldBe('A6', valueS('$4,090.91')),
      shouldBe('A7', valueS('$409.09')),

      exec(done)
    ]);
  });
  it('TBILLEQ', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A2', '3/31/2008'),
      excel('A3', '6/1/2008'),
      excel('A4', '9.14%'),
      excel('A5', 'Formula'),
      excel('A6', '=TBILLEQ(A2,A3,A4)'),
      excel('B1', 'Description'),
      excel('B2', 'Settlement date'),
      excel('B3', 'Maturity date'),
      excel('B4', 'Percent discount rate'),
      excel('B5', 'Description'),
      excel('B6', 'The bond equivalent yield, for the Treasury bill using the terms in A2, A3, and A4 (0.09415, or 9.42%).'),
      excel('C5', 'Result'),

      shouldBe('A6', valueS('9.42%')),

      exec(done)
    ]);
  });
  it('TBILLPRICE', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A2', '3/31/2008'),
      excel('A3', '6/1/2008'),
      excel('A4', '9.0%'),
      excel('A5', 'Formula'),
      excel('A6', '=TBILLPRICE(A2,A3,A4)'),
      excel('B1', 'Description'),
      excel('B2', 'Settlement date'),
      excel('B3', 'Maturity date'),
      excel('B4', 'Percent discount rate'),
      excel('B5', 'Description'),
      excel('B6', 'The price for the Treasury bill, using the terms in A2, A3, and A4.'),
      excel('C5', 'Result'),

      shouldBe('A6', valueS('$    98.45')),

      exec(done)
    ]);
  });
  it('TBILLYIELD', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A2', '3/31/2008'),
      excel('A3', '6/1/2008'),
      excel('A4', '$98.45'),
      excel('A5', 'Formula'),
      excel('A6', '=TBILLYIELD(A2,A3,A4)'),
      excel('B1', 'Description'),
      excel('B2', 'Settlement date'),
      excel('B3', 'Maturity date'),
      excel('B4', 'Price per $100 face value'),
      excel('B5', 'Description'),
      excel('B6', 'The yield for the Treasury bill using the terms in A2, A3, and A4 (0.0914, or 9.14%).'),
      excel('C5', 'Result'),

      shouldBe('A6', valueS('9.14%')),

      exec(done)
    ]);
  });
  it('VDB', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A10', '=VDB(A2, A3, A4*12, 6, 18, 1.5)'),
      excel('A11', '=VDB(A2, A3, A4, 0, 0.875, 1.5)'),
      excel('A2', '2400'),
      excel('A3', '300'),
      excel('A4', '10'),
      excel('A5', 'Formula'),
      excel('A6', '=VDB(A2, A3, A4*365, 0, 1)'),
      excel('A7', '=VDB(A2, A3, A4*12, 0, 1)'),
      excel('A8', '=VDB(A2, A3, A4, 0, 1)'),
      excel('A9', '=VDB(A2, A3, A4*12, 6, 18)'),
      excel('B1', 'Description'),
      excel('B10', 'Depreciation between the sixth month and the eighteenth month using a factor of 1.5 instead of the double-declining balance method.'),
      excel('B11', 'Depreciation for the first fiscal year that you own the asset, assuming that tax laws limit you to 150-percent depreciation of the declining balance. Asset is purchased in the middle of the first quarter of the fiscal year.'),
      excel('B2', 'Initial cost'),
      excel('B3', 'Salvage value'),
      excel('B4', 'Lifetime in years'),
      excel('B5', 'Description'),
      excel('B6', 'First day\'s depreciation. Excel automatically assumes that factor is 2.'),
      excel('B7', 'First month\'s depreciation.'),
      excel('B8', 'First year\'s depreciation.'),
      excel('B9', 'Depreciation between the sixth month and the eighteenth month.'),
      excel('C5', 'Result'),

      shouldBe('A10', valueS('$311.81')),
      shouldBe('A11', valueS('$315.00')),
      shouldBe('A6', valueS('$1.32')),
      shouldBe('A7', valueS('$40.00')),
      shouldBe('A8', valueS('$480.00')),
      shouldBe('A9', valueS('$396.31')),

      exec(done)
    ]);
  });
  it('XIRR', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A2', 'Values'),
      excel('A3', '-10,000'),
      excel('A4', '2,750'),
      excel('A5', '4,250'),
      excel('A6', '3,250'),
      excel('A7', '2,750'),
      excel('A8', 'Formula'),
      excel('A9', '=XIRR(A3:A7, B3:B7, 0.1)'),
      excel('B2', 'Dates'),
      excel('B3', '1-Jan-08'),
      excel('B4', '1-Mar-08'),
      excel('B5', '30-Oct-08'),
      excel('B6', '15-Feb-09'),
      excel('B7', '1-Apr-09'),
      excel('B8', 'Description (Result)'),
      excel('B9', 'The internal rate of return (0.373362535 or 37.34%)'),
      excel('C8', 'Result'),

      shouldBe('A9', valueS('37.34%')),

      exec(done)
    ]);
  });
  it('XNPV', (done) => {
    _do([
      excel('A1', 'Values'),
      excel('A2', '-$10,000'),
      excel('A3', '$2,750'),
      excel('A4', '$4,250'),
      excel('A5', '$3,250'),
      excel('A6', '$2,750'),
      excel('A7', 'Formula'),
      excel('A8', '=XNPV(.09, A2:A6, B2:B6)'),
      excel('B1', 'Dates'),
      excel('B2', '1/1/2008'),
      excel('B3', '3/1/2008'),
      excel('B4', '10/30/2008'),
      excel('B5', '2/15/2009'),
      excel('B6', '4/1/2009'),
      excel('B7', 'Description'),
      excel('B8', 'The net present value for an investment with the above cost and returns. The cash flows are discounted at 9 percent.'),
      excel('C7', 'Result'),

      shouldBe('A8', valueS('$2,086.65')),

      exec(done)
    ]);
  });
  it('YIELD', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A10', '=YIELD(A2,A3,A4,A5,A6,A7,A8)'),
      excel('A2', '15-Feb-08'),
      excel('A3', '15-Nov-16'),
      excel('A4', '5.75%'),
      excel('A5', '95.04287'),
      excel('A6', '$100'),
      excel('A7', '2'),
      excel('A8', '0'),
      excel('A9', 'Formula'),
      excel('B1', 'Description'),
      excel('B10', 'The yield, for the bond with the terms above (0.065 or 6.5%)'),
      excel('B2', 'Settlement date'),
      excel('B3', 'Maturity date'),
      excel('B4', 'Percent coupon'),
      excel('B5', 'Price'),
      excel('B6', 'Redemption value'),
      excel('B7', 'Frequency is semiannual (see above)'),
      excel('B8', '30/360 basis (see above)'),
      excel('B9', 'Description (Result)'),
      excel('C9', 'Result'),

      shouldBe('A10', valueS('6.5%')),

      exec(done)
    ]);
  });
  it('YIELDDISC', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A2', '16-Feb-08'),
      excel('A3', '1-Mar-08'),
      excel('A4', '99.795'),
      excel('A5', '$100'),
      excel('A6', '2'),
      excel('A7', 'Formula'),
      excel('A8', '=YIELDDISC(A2,A3,A4,A5,A6)'),
      excel('B2', 'Settlement date'),
      excel('B3', 'Maturity date'),
      excel('B4', 'Price'),
      excel('B5', 'Redemption value'),
      excel('B6', 'Actual/360 basis'),
      excel('B7', 'Description (Result)'),
      excel('B8', 'The yield, for the bond with the terms above (0.052823 or 5.28%)'),
      excel('C7', 'Result'),

      shouldBe('A8', valueD(0.052823)),

      exec(done)
    ]);
  });
  it('YIELDMAT', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A2', '15-Mar-08'),
      excel('A3', '3-Nov-08'),
      excel('A4', '8-Nov-07'),
      excel('A5', '6.25%'),
      excel('A6', '100.0123'),
      excel('A7', '0'),
      excel('A8', 'Formula'),
      excel('A9', '=YIELDMAT(A2,A3,A4,A5,A6,A7)'),
      excel('B1', 'Description'),
      excel('B2', 'Settlement date'),
      excel('B3', 'Maturity date'),
      excel('B4', 'Issue date'),
      excel('B5', 'Percent semiannual coupon'),
      excel('B6', 'Price'),
      excel('B7', '30/360 basis (see above)'),
      excel('B8', 'Description (Result)'),
      excel('B9', 'The yield, for the bond with the terms above (0.060954 or 6.10%)'),
      excel('C8', 'Result'),

      shouldBe('A9', valueS('6.10%')),

      exec(done)
    ]);
  });
  it('CELL', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A2', '75'),
      excel('A3', 'Hello, world!'),
      excel('A4', 'Formula'),
      excel('A5', '=CELL("row", A20)'),
      excel('A6', '=CELL("contents", A3)'),
      excel('A7', '=CELL("type", A2)'),
      excel('B4', 'Description'),
      excel('B5', 'The row number of cell A20.'),
      excel('B6', 'The contents of cell A3.'),
      excel('B7', 'The data type of cell A2. A data type of "v" indicates value.'),
      excel('C4', 'Result'),

      shouldBe('A5', valueI(20)),
      shouldBe('A6', valueS('Hello, world!')),
      shouldBe('A7', valueS('v')),

      exec(done)
    ]);
  });
  it('ERRORTYPE', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A2', '#NULL!'),
      excel('A3', '#DIV/0!'),
      excel('A4', 'Formula'),
      excel('A5', '=ERROR.TYPE(A2)'),
      excel('A6', '=IF(ERROR.TYPE(A3)<3,CHOOSE(ERROR.TYPE(A3),"Ranges do not intersect","The divisor is zero"))'),
      excel('B4', 'Description'),
      excel('B5', 'Number of the #NULL! Error(1).'),
      excel('B6', 'Checks cell A3 to see whether the cell contains either the #NULL! error value or the #DIV/0! error value. If it does, then the number for the error value is used in the CHOOSE worksheet function to display one of two messages; otherwise, the #N/A error value is returned.'),
      excel('C4', 'Result'),

      shouldBe('A5', valueI(1)),
      shouldBe('A6', valueS('The divisor is zero')),

      exec(done)
    ]);
  });
  it('INFO', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '\'=INFO("numfile")'),
      excel('A3', '\'=INFO("recalc")'),
      excel('B1', 'Description'),
      excel('B2', 'Number of active worksheets'),
      excel('B3', 'Recalculation mode for the workbook.'),
      excel('C1', 'Result'),
      excel('C2', 'INFO("numfile")'),

      shouldBe('A3', valueS('=INFO("recalc")')),

      exec(done)
    ]);
  });
  it('ISBLANK', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=ISLOGICAL(TRUE)'),
      excel('A3', '=ISLOGICAL("TRUE")'),
      excel('A4', '=ISNUMBER(4)'),
      excel('A5', '=ISREF(G8)'),
      excel('A6', '=ISREF(XYZ1)'),
      excel('B1', 'Description'),
      excel('B2', 'Checks whether TRUE is a logical value'),
      excel('B3', 'Checks whether "TRUE" is a logical value'),
      excel('B4', 'Checks whether 4 is a number'),
      excel('B5', 'Checks whether G8 is a valid reference'),
      excel('B6', 'Checks whether XYZ1 is a valid reference'),
      excel('C1', 'Result'),

      shouldBe('A2', valueB(true)),
      shouldBe('A3', valueB(false)),
      shouldBe('A4', valueB(true)),
      shouldBe('A5', valueB(true)),
      shouldBe('A6', valueB(false)),

      exec(done)
    ]);
  });
  it('ISBLANK', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A10', '=ISNA(A4)'),
      excel('A11', '=ISNA(A6)'),
      excel('A12', '=ISERR(A6)'),
      excel('A13', '=ISNUMBER(A5)'),
      excel('A14', '=ISTEXT(A3)'),
      excel('A2', 'Gold'),
      excel('A3', 'Region1'),
      excel('A4', '#REF!'),
      excel('A5', '330.92'),
      excel('A6', '#N/A'),
      excel('A7', 'Formula'),
      excel('A8', '=ISBLANK(A2)'),
      excel('A9', '=ISERROR(A4)'),
      excel('B10', 'Checks whether the value in cell A4, #REF!, is the #N/A error.'),
      excel('B11', 'Checks whether the value in cell A6, #N/A, is the #N/A error.'),
      excel('B12', 'Checks whether the value in cell A6, #N/A, is an error.'),
      excel('B13', 'Checks whether the value in cell A5, 330.92, is a number.'),
      excel('B14', 'Checks whether the value in cell A3, Region1, is text.'),
      excel('B7', 'Description'),
      excel('B8', 'Checks whether cell A2 is blank.'),
      excel('B9', 'Checks whether the value in cell A4, #REF!, is an error.'),
      excel('C7', 'Result'),

      shouldBe('A10', valueB(false)),
      shouldBe('A11', valueB(true)),
      shouldBe('A12', valueB(false)),
      shouldBe('A13', valueB(true)),
      shouldBe('A14', valueB(true)),
      shouldBe('A8', valueB(false)),
      shouldBe('A9', valueB(true)),

      exec(done)
    ]);
  });
  it('ISERR', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=ISLOGICAL(TRUE)'),
      excel('A3', '=ISLOGICAL("TRUE")'),
      excel('A4', '=ISNUMBER(4)'),
      excel('A5', '=ISREF(G8)'),
      excel('A6', '=ISREF(XYZ1)'),
      excel('B1', 'Description'),
      excel('B2', 'Checks whether TRUE is a logical value'),
      excel('B3', 'Checks whether "TRUE" is a logical value'),
      excel('B4', 'Checks whether 4 is a number'),
      excel('B5', 'Checks whether G8 is a valid reference'),
      excel('B6', 'Checks whether XYZ1 is a valid reference'),
      excel('C1', 'Result'),

      shouldBe('A2', valueB(true)),
      shouldBe('A3', valueB(false)),
      shouldBe('A4', valueB(true)),
      shouldBe('A5', valueB(true)),
      shouldBe('A6', valueB(false)),

      exec(done)
    ]);
  });
  it('ISERR', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A10', '=ISNA(A4)'),
      excel('A11', '=ISNA(A6)'),
      excel('A12', '=ISERR(A6)'),
      excel('A13', '=ISNUMBER(A5)'),
      excel('A14', '=ISTEXT(A3)'),
      excel('A2', 'Gold'),
      excel('A3', 'Region1'),
      excel('A4', '#REF!'),
      excel('A5', '330.92'),
      excel('A6', '#N/A'),
      excel('A7', 'Formula'),
      excel('A8', '=ISBLANK(A2)'),
      excel('A9', '=ISERROR(A4)'),
      excel('B10', 'Checks whether the value in cell A4, #REF!, is the #N/A error.'),
      excel('B11', 'Checks whether the value in cell A6, #N/A, is the #N/A error.'),
      excel('B12', 'Checks whether the value in cell A6, #N/A, is an error.'),
      excel('B13', 'Checks whether the value in cell A5, 330.92, is a number.'),
      excel('B14', 'Checks whether the value in cell A3, Region1, is text.'),
      excel('B7', 'Description'),
      excel('B8', 'Checks whether cell A2 is blank.'),
      excel('B9', 'Checks whether the value in cell A4, #REF!, is an error.'),
      excel('C7', 'Result'),

      shouldBe('A10', valueB(false)),
      shouldBe('A11', valueB(true)),
      shouldBe('A12', valueB(false)),
      shouldBe('A13', valueB(true)),
      shouldBe('A14', valueB(true)),
      shouldBe('A8', valueB(false)),
      shouldBe('A9', valueB(true)),

      exec(done)
    ]);
  });
  it('ISERROR', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=ISLOGICAL(TRUE)'),
      excel('A3', '=ISLOGICAL("TRUE")'),
      excel('A4', '=ISNUMBER(4)'),
      excel('A5', '=ISREF(G8)'),
      excel('A6', '=ISREF(XYZ1)'),
      excel('B1', 'Description'),
      excel('B2', 'Checks whether TRUE is a logical value'),
      excel('B3', 'Checks whether "TRUE" is a logical value'),
      excel('B4', 'Checks whether 4 is a number'),
      excel('B5', 'Checks whether G8 is a valid reference'),
      excel('B6', 'Checks whether XYZ1 is a valid reference'),
      excel('C1', 'Result'),

      shouldBe('A2', valueB(true)),
      shouldBe('A3', valueB(false)),
      shouldBe('A4', valueB(true)),
      shouldBe('A5', valueB(true)),
      shouldBe('A6', valueB(false)),

      exec(done)
    ]);
  });
  it('ISERROR', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A10', '=ISNA(A4)'),
      excel('A11', '=ISNA(A6)'),
      excel('A12', '=ISERR(A6)'),
      excel('A13', '=ISNUMBER(A5)'),
      excel('A14', '=ISTEXT(A3)'),
      excel('A2', 'Gold'),
      excel('A3', 'Region1'),
      excel('A4', '#REF!'),
      excel('A5', '330.92'),
      excel('A6', '#N/A'),
      excel('A7', 'Formula'),
      excel('A8', '=ISBLANK(A2)'),
      excel('A9', '=ISERROR(A4)'),
      excel('B10', 'Checks whether the value in cell A4, #REF!, is the #N/A error.'),
      excel('B11', 'Checks whether the value in cell A6, #N/A, is the #N/A error.'),
      excel('B12', 'Checks whether the value in cell A6, #N/A, is an error.'),
      excel('B13', 'Checks whether the value in cell A5, 330.92, is a number.'),
      excel('B14', 'Checks whether the value in cell A3, Region1, is text.'),
      excel('B7', 'Description'),
      excel('B8', 'Checks whether cell A2 is blank.'),
      excel('B9', 'Checks whether the value in cell A4, #REF!, is an error.'),
      excel('C7', 'Result'),

      shouldBe('A10', valueB(false)),
      shouldBe('A11', valueB(true)),
      shouldBe('A12', valueB(false)),
      shouldBe('A13', valueB(true)),
      shouldBe('A14', valueB(true)),
      shouldBe('A8', valueB(false)),
      shouldBe('A9', valueB(true)),

      exec(done)
    ]);
  });
  it('ISEVEN', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=ISEVEN(-1)'),
      excel('A3', '=ISEVEN(2.5)'),
      excel('A4', '=ISEVEN(5)'),
      excel('A5', '=ISEVEN(0)'),
      excel('A6', '12/23/2011'),
      excel('B1', 'Description'),
      excel('B2', 'Tests whether -1 is even'),
      excel('B3', 'Checks whether 2.5 is even. The decimal portion, .5, is truncated, so 2 is tested.'),
      excel('B4', 'Tests whether 5 is even.'),
      excel('B5', 'Zero (0) is considered even.'),
      excel('B6', 'Tests the date in A6. The decimal representation of 12/23/2011 is 40900.'),
      excel('C1', 'Result'),
      excel('C6', 'TRUE'),

      shouldBe('A2', valueB(false)),
      shouldBe('A3', valueB(true)),
      shouldBe('A4', valueB(false)),
      shouldBe('A5', valueB(true)),

      exec(done)
    ]);
  });
  it('ISFORMULA', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=TODAY()'),
      excel('A3', '7'),
      excel('A4', 'Hello, world!'),
      excel('A5', '=3/0'),
      excel('B1', 'Description'),
      excel('B2', 'Returns TRUE because =TODAY() is a formula.'),
      excel('B3', 'Returns FALSE because 7 is a number, not a formula.'),
      excel('B4', 'Returns FALSE because "Hello, world!" is text, not a formula.'),
      excel('B5', 'Returns TRUE because, although dividing by 0 results in an error, the cell does contain a formula.'),
      excel('C1', 'Result'),
      excel('C3', 'FALSE'),
      excel('C4', 'FALSE'),

      shouldBe('A2', valueB(true)),
      shouldBe('A5', valueB(true)),

      exec(done)
    ]);
  });
  it('ISLOGICAL', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=ISLOGICAL(TRUE)'),
      excel('A3', '=ISLOGICAL("TRUE")'),
      excel('A4', '=ISNUMBER(4)'),
      excel('A5', '=ISREF(G8)'),
      excel('A6', '=ISREF(XYZ1)'),
      excel('B1', 'Description'),
      excel('B2', 'Checks whether TRUE is a logical value'),
      excel('B3', 'Checks whether "TRUE" is a logical value'),
      excel('B4', 'Checks whether 4 is a number'),
      excel('B5', 'Checks whether G8 is a valid reference'),
      excel('B6', 'Checks whether XYZ1 is a valid reference'),
      excel('C1', 'Result'),

      shouldBe('A2', valueB(true)),
      shouldBe('A3', valueB(false)),
      shouldBe('A4', valueB(true)),
      shouldBe('A5', valueB(true)),
      shouldBe('A6', valueB(false)),

      exec(done)
    ]);
  });
  it('ISLOGICAL', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A10', '=ISNA(A4)'),
      excel('A11', '=ISNA(A6)'),
      excel('A12', '=ISERR(A6)'),
      excel('A13', '=ISNUMBER(A5)'),
      excel('A14', '=ISTEXT(A3)'),
      excel('A2', 'Gold'),
      excel('A3', 'Region1'),
      excel('A4', '#REF!'),
      excel('A5', '330.92'),
      excel('A6', '#N/A'),
      excel('A7', 'Formula'),
      excel('A8', '=ISBLANK(A2)'),
      excel('A9', '=ISERROR(A4)'),
      excel('B10', 'Checks whether the value in cell A4, #REF!, is the #N/A error.'),
      excel('B11', 'Checks whether the value in cell A6, #N/A, is the #N/A error.'),
      excel('B12', 'Checks whether the value in cell A6, #N/A, is an error.'),
      excel('B13', 'Checks whether the value in cell A5, 330.92, is a number.'),
      excel('B14', 'Checks whether the value in cell A3, Region1, is text.'),
      excel('B7', 'Description'),
      excel('B8', 'Checks whether cell A2 is blank.'),
      excel('B9', 'Checks whether the value in cell A4, #REF!, is an error.'),
      excel('C7', 'Result'),

      shouldBe('A10', valueB(false)),
      shouldBe('A11', valueB(true)),
      shouldBe('A12', valueB(false)),
      shouldBe('A13', valueB(true)),
      shouldBe('A14', valueB(true)),
      shouldBe('A8', valueB(false)),
      shouldBe('A9', valueB(true)),

      exec(done)
    ]);
  });
  it('ISNA', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=ISLOGICAL(TRUE)'),
      excel('A3', '=ISLOGICAL("TRUE")'),
      excel('A4', '=ISNUMBER(4)'),
      excel('A5', '=ISREF(G8)'),
      excel('A6', '=ISREF(XYZ1)'),
      excel('B1', 'Description'),
      excel('B2', 'Checks whether TRUE is a logical value'),
      excel('B3', 'Checks whether "TRUE" is a logical value'),
      excel('B4', 'Checks whether 4 is a number'),
      excel('B5', 'Checks whether G8 is a valid reference'),
      excel('B6', 'Checks whether XYZ1 is a valid reference'),
      excel('C1', 'Result'),

      shouldBe('A2', valueB(true)),
      shouldBe('A3', valueB(false)),
      shouldBe('A4', valueB(true)),
      shouldBe('A5', valueB(true)),
      shouldBe('A6', valueB(false)),

      exec(done)
    ]);
  });
  it('ISNA', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A10', '=ISNA(A4)'),
      excel('A11', '=ISNA(A6)'),
      excel('A12', '=ISERR(A6)'),
      excel('A13', '=ISNUMBER(A5)'),
      excel('A14', '=ISTEXT(A3)'),
      excel('A2', 'Gold'),
      excel('A3', 'Region1'),
      excel('A4', '#REF!'),
      excel('A5', '330.92'),
      excel('A6', '#N/A'),
      excel('A7', 'Formula'),
      excel('A8', '=ISBLANK(A2)'),
      excel('A9', '=ISERROR(A4)'),
      excel('B10', 'Checks whether the value in cell A4, #REF!, is the #N/A error.'),
      excel('B11', 'Checks whether the value in cell A6, #N/A, is the #N/A error.'),
      excel('B12', 'Checks whether the value in cell A6, #N/A, is an error.'),
      excel('B13', 'Checks whether the value in cell A5, 330.92, is a number.'),
      excel('B14', 'Checks whether the value in cell A3, Region1, is text.'),
      excel('B7', 'Description'),
      excel('B8', 'Checks whether cell A2 is blank.'),
      excel('B9', 'Checks whether the value in cell A4, #REF!, is an error.'),
      excel('C7', 'Result'),

      shouldBe('A10', valueB(false)),
      shouldBe('A11', valueB(true)),
      shouldBe('A12', valueB(false)),
      shouldBe('A13', valueB(true)),
      shouldBe('A14', valueB(true)),
      shouldBe('A8', valueB(false)),
      shouldBe('A9', valueB(true)),

      exec(done)
    ]);
  });
  it('ISNONTEXT', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=ISLOGICAL(TRUE)'),
      excel('A3', '=ISLOGICAL("TRUE")'),
      excel('A4', '=ISNUMBER(4)'),
      excel('A5', '=ISREF(G8)'),
      excel('A6', '=ISREF(XYZ1)'),
      excel('B1', 'Description'),
      excel('B2', 'Checks whether TRUE is a logical value'),
      excel('B3', 'Checks whether "TRUE" is a logical value'),
      excel('B4', 'Checks whether 4 is a number'),
      excel('B5', 'Checks whether G8 is a valid reference'),
      excel('B6', 'Checks whether XYZ1 is a valid reference'),
      excel('C1', 'Result'),

      shouldBe('A2', valueB(true)),
      shouldBe('A3', valueB(false)),
      shouldBe('A4', valueB(true)),
      shouldBe('A5', valueB(true)),
      shouldBe('A6', valueB(false)),

      exec(done)
    ]);
  });
  it('ISNONTEXT', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A10', '=ISNA(A4)'),
      excel('A11', '=ISNA(A6)'),
      excel('A12', '=ISERR(A6)'),
      excel('A13', '=ISNUMBER(A5)'),
      excel('A14', '=ISTEXT(A3)'),
      excel('A2', 'Gold'),
      excel('A3', 'Region1'),
      excel('A4', '#REF!'),
      excel('A5', '330.92'),
      excel('A6', '#N/A'),
      excel('A7', 'Formula'),
      excel('A8', '=ISBLANK(A2)'),
      excel('A9', '=ISERROR(A4)'),
      excel('B10', 'Checks whether the value in cell A4, #REF!, is the #N/A error.'),
      excel('B11', 'Checks whether the value in cell A6, #N/A, is the #N/A error.'),
      excel('B12', 'Checks whether the value in cell A6, #N/A, is an error.'),
      excel('B13', 'Checks whether the value in cell A5, 330.92, is a number.'),
      excel('B14', 'Checks whether the value in cell A3, Region1, is text.'),
      excel('B7', 'Description'),
      excel('B8', 'Checks whether cell A2 is blank.'),
      excel('B9', 'Checks whether the value in cell A4, #REF!, is an error.'),
      excel('C7', 'Result'),

      shouldBe('A10', valueB(false)),
      shouldBe('A11', valueB(true)),
      shouldBe('A12', valueB(false)),
      shouldBe('A13', valueB(true)),
      shouldBe('A14', valueB(true)),
      shouldBe('A8', valueB(false)),
      shouldBe('A9', valueB(true)),

      exec(done)
    ]);
  });
  it('ISNUMBER', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=ISLOGICAL(TRUE)'),
      excel('A3', '=ISLOGICAL("TRUE")'),
      excel('A4', '=ISNUMBER(4)'),
      excel('A5', '=ISREF(G8)'),
      excel('A6', '=ISREF(XYZ1)'),
      excel('B1', 'Description'),
      excel('B2', 'Checks whether TRUE is a logical value'),
      excel('B3', 'Checks whether "TRUE" is a logical value'),
      excel('B4', 'Checks whether 4 is a number'),
      excel('B5', 'Checks whether G8 is a valid reference'),
      excel('B6', 'Checks whether XYZ1 is a valid reference'),
      excel('C1', 'Result'),

      shouldBe('A2', valueB(true)),
      shouldBe('A3', valueB(false)),
      shouldBe('A4', valueB(true)),
      shouldBe('A5', valueB(true)),
      shouldBe('A6', valueB(false)),

      exec(done)
    ]);
  });
  it('ISNUMBER', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A10', '=ISNA(A4)'),
      excel('A11', '=ISNA(A6)'),
      excel('A12', '=ISERR(A6)'),
      excel('A13', '=ISNUMBER(A5)'),
      excel('A14', '=ISTEXT(A3)'),
      excel('A2', 'Gold'),
      excel('A3', 'Region1'),
      excel('A4', '#REF!'),
      excel('A5', '330.92'),
      excel('A6', '#N/A'),
      excel('A7', 'Formula'),
      excel('A8', '=ISBLANK(A2)'),
      excel('A9', '=ISERROR(A4)'),
      excel('B10', 'Checks whether the value in cell A4, #REF!, is the #N/A error.'),
      excel('B11', 'Checks whether the value in cell A6, #N/A, is the #N/A error.'),
      excel('B12', 'Checks whether the value in cell A6, #N/A, is an error.'),
      excel('B13', 'Checks whether the value in cell A5, 330.92, is a number.'),
      excel('B14', 'Checks whether the value in cell A3, Region1, is text.'),
      excel('B7', 'Description'),
      excel('B8', 'Checks whether cell A2 is blank.'),
      excel('B9', 'Checks whether the value in cell A4, #REF!, is an error.'),
      excel('C7', 'Result'),

      shouldBe('A10', valueB(false)),
      shouldBe('A11', valueB(true)),
      shouldBe('A12', valueB(false)),
      shouldBe('A13', valueB(true)),
      shouldBe('A14', valueB(true)),
      shouldBe('A8', valueB(false)),
      shouldBe('A9', valueB(true)),

      exec(done)
    ]);
  });
  it('ISODD', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=ISLOGICAL(TRUE)'),
      excel('A3', '=ISLOGICAL("TRUE")'),
      excel('A4', '=ISNUMBER(4)'),
      excel('A5', '=ISREF(G8)'),
      excel('A6', '=ISREF(XYZ1)'),
      excel('B1', 'Description'),
      excel('B2', 'Checks whether TRUE is a logical value'),
      excel('B3', 'Checks whether "TRUE" is a logical value'),
      excel('B4', 'Checks whether 4 is a number'),
      excel('B5', 'Checks whether G8 is a valid reference'),
      excel('B6', 'Checks whether XYZ1 is a valid reference'),
      excel('C1', 'Result'),

      shouldBe('A2', valueB(true)),
      shouldBe('A3', valueB(false)),
      shouldBe('A4', valueB(true)),
      shouldBe('A5', valueB(true)),
      shouldBe('A6', valueB(false)),

      exec(done)
    ]);
  });
  it('ISODD', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A10', '=ISNA(A4)'),
      excel('A11', '=ISNA(A6)'),
      excel('A12', '=ISERR(A6)'),
      excel('A13', '=ISNUMBER(A5)'),
      excel('A14', '=ISTEXT(A3)'),
      excel('A2', 'Gold'),
      excel('A3', 'Region1'),
      excel('A4', '#REF!'),
      excel('A5', '330.92'),
      excel('A6', '#N/A'),
      excel('A7', 'Formula'),
      excel('A8', '=ISBLANK(A2)'),
      excel('A9', '=ISERROR(A4)'),
      excel('B10', 'Checks whether the value in cell A4, #REF!, is the #N/A error.'),
      excel('B11', 'Checks whether the value in cell A6, #N/A, is the #N/A error.'),
      excel('B12', 'Checks whether the value in cell A6, #N/A, is an error.'),
      excel('B13', 'Checks whether the value in cell A5, 330.92, is a number.'),
      excel('B14', 'Checks whether the value in cell A3, Region1, is text.'),
      excel('B7', 'Description'),
      excel('B8', 'Checks whether cell A2 is blank.'),
      excel('B9', 'Checks whether the value in cell A4, #REF!, is an error.'),
      excel('C7', 'Result'),

      shouldBe('A10', valueB(false)),
      shouldBe('A11', valueB(true)),
      shouldBe('A12', valueB(false)),
      shouldBe('A13', valueB(true)),
      shouldBe('A14', valueB(true)),
      shouldBe('A8', valueB(false)),
      shouldBe('A9', valueB(true)),

      exec(done)
    ]);
  });
  it('ISREF', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=ISLOGICAL(TRUE)'),
      excel('A3', '=ISLOGICAL("TRUE")'),
      excel('A4', '=ISNUMBER(4)'),
      excel('A5', '=ISREF(G8)'),
      excel('A6', '=ISREF(XYZ1)'),
      excel('B1', 'Description'),
      excel('B2', 'Checks whether TRUE is a logical value'),
      excel('B3', 'Checks whether "TRUE" is a logical value'),
      excel('B4', 'Checks whether 4 is a number'),
      excel('B5', 'Checks whether G8 is a valid reference'),
      excel('B6', 'Checks whether XYZ1 is a valid reference'),
      excel('C1', 'Result'),

      shouldBe('A2', valueB(true)),
      shouldBe('A3', valueB(false)),
      shouldBe('A4', valueB(true)),
      shouldBe('A5', valueB(true)),
      shouldBe('A6', valueB(false)),

      exec(done)
    ]);
  });
  it('ISREF', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A10', '=ISNA(A4)'),
      excel('A11', '=ISNA(A6)'),
      excel('A12', '=ISERR(A6)'),
      excel('A13', '=ISNUMBER(A5)'),
      excel('A14', '=ISTEXT(A3)'),
      excel('A2', 'Gold'),
      excel('A3', 'Region1'),
      excel('A4', '#REF!'),
      excel('A5', '330.92'),
      excel('A6', '#N/A'),
      excel('A7', 'Formula'),
      excel('A8', '=ISBLANK(A2)'),
      excel('A9', '=ISERROR(A4)'),
      excel('B10', 'Checks whether the value in cell A4, #REF!, is the #N/A error.'),
      excel('B11', 'Checks whether the value in cell A6, #N/A, is the #N/A error.'),
      excel('B12', 'Checks whether the value in cell A6, #N/A, is an error.'),
      excel('B13', 'Checks whether the value in cell A5, 330.92, is a number.'),
      excel('B14', 'Checks whether the value in cell A3, Region1, is text.'),
      excel('B7', 'Description'),
      excel('B8', 'Checks whether cell A2 is blank.'),
      excel('B9', 'Checks whether the value in cell A4, #REF!, is an error.'),
      excel('C7', 'Result'),

      shouldBe('A10', valueB(false)),
      shouldBe('A11', valueB(true)),
      shouldBe('A12', valueB(false)),
      shouldBe('A13', valueB(true)),
      shouldBe('A14', valueB(true)),
      shouldBe('A8', valueB(false)),
      shouldBe('A9', valueB(true)),

      exec(done)
    ]);
  });
  it('ISTEXT', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=ISLOGICAL(TRUE)'),
      excel('A3', '=ISLOGICAL("TRUE")'),
      excel('A4', '=ISNUMBER(4)'),
      excel('A5', '=ISREF(G8)'),
      excel('A6', '=ISREF(XYZ1)'),
      excel('B1', 'Description'),
      excel('B2', 'Checks whether TRUE is a logical value'),
      excel('B3', 'Checks whether "TRUE" is a logical value'),
      excel('B4', 'Checks whether 4 is a number'),
      excel('B5', 'Checks whether G8 is a valid reference'),
      excel('B6', 'Checks whether XYZ1 is a valid reference'),
      excel('C1', 'Result'),

      shouldBe('A2', valueB(true)),
      shouldBe('A3', valueB(false)),
      shouldBe('A4', valueB(true)),
      shouldBe('A5', valueB(true)),
      shouldBe('A6', valueB(false)),

      exec(done)
    ]);
  });
  it('ISTEXT', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A10', '=ISNA(A4)'),
      excel('A11', '=ISNA(A6)'),
      excel('A12', '=ISERR(A6)'),
      excel('A13', '=ISNUMBER(A5)'),
      excel('A14', '=ISTEXT(A3)'),
      excel('A2', 'Gold'),
      excel('A3', 'Region1'),
      excel('A4', '#REF!'),
      excel('A5', '330.92'),
      excel('A6', '#N/A'),
      excel('A7', 'Formula'),
      excel('A8', '=ISBLANK(A2)'),
      excel('A9', '=ISERROR(A4)'),
      excel('B10', 'Checks whether the value in cell A4, #REF!, is the #N/A error.'),
      excel('B11', 'Checks whether the value in cell A6, #N/A, is the #N/A error.'),
      excel('B12', 'Checks whether the value in cell A6, #N/A, is an error.'),
      excel('B13', 'Checks whether the value in cell A5, 330.92, is a number.'),
      excel('B14', 'Checks whether the value in cell A3, Region1, is text.'),
      excel('B7', 'Description'),
      excel('B8', 'Checks whether cell A2 is blank.'),
      excel('B9', 'Checks whether the value in cell A4, #REF!, is an error.'),
      excel('C7', 'Result'),

      shouldBe('A10', valueB(false)),
      shouldBe('A11', valueB(true)),
      shouldBe('A12', valueB(false)),
      shouldBe('A13', valueB(true)),
      shouldBe('A14', valueB(true)),
      shouldBe('A8', valueB(false)),
      shouldBe('A9', valueB(true)),

      exec(done)
    ]);
  });
  it('N', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A10', '=N(A5)'),
      excel('A11', '=N("7")'),
      excel('A2', '7'),
      excel('A3', 'Even'),
      excel('A4', 'TRUE'),
      excel('A5', '4/17/2011'),
      excel('A6', 'Formula'),
      excel('A7', '=N(A2)'),
      excel('A8', '=N(A3)'),
      excel('A9', '=N(A4)'),
      excel('B10', 'Because A5 is a date, the serial number of the date is returned (varies with the date system used).'),
      excel('B11', 'Because "7" is text, 0 is returned.'),
      excel('B6', 'Description'),
      excel('B7', 'Because A2 contains a number, the number is returned.'),
      excel('B8', 'Because A3 contains text, 0 is returned.'),
      excel('B9', 'Because A4 is the logical value TRUE, 1 is returned.'),
      excel('C6', 'Result'),

      shouldBe('A10', valueI(40650)),
      shouldBe('A11', valueI(0)),
      shouldBe('A7', valueI(7)),
      shouldBe('A8', valueI(0)),
      shouldBe('A9', valueI(1)),

      exec(done)
    ]);
  });
  it('SHEET', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=SHEET(QSalesByRegion)'),
      excel('A3', '=SHEET(Table1)'),
      excel('A4', '=SHEET(Hi_Temps)'),
      excel('A5', '=SHEET("Stuff")'),
      excel('B1', 'Description'),
      excel('B2', 'Returns the sheet number that contains the defined name QSalesByRegion on Sheet2, and has a scope that makes it available to the entire workbook.'),
      excel('B3', 'Returns the sheet number that contains the table named Table1 on Sheet2, and has a scope that makes it available to the entire workbook.'),
      excel('B4', 'Returns the #NAME? error value because the defined name Hi_Temps is limited to the worksheet that contains it, Sheet2.'),
      excel('B5', 'Returns the sheet number of the worksheet named Stuff.'),
      excel('C1', 'Result'),

      shouldBe('A2', valueI(2)),
      shouldBe('A3', valueI(2)),
      shouldBe('A4', valueS('#NAME?')),
      shouldBe('A5', valueI(3)),

      exec(done)
    ]);
  });
  it('SHEETS', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=SHEETS()'),
      excel('A3', '=SHEETS(My3DRef)'),
      excel('B1', 'Description'),
      excel('B2', 'Because there is no Reference argument specified, the total number of sheets in the workbook is returned (3).'),
      excel('B3', 'Returns the number of sheets in a 3D reference with the defined name My3DRef, which includes Sheet2 and Sheet3 (2).'),
      excel('C1', 'Result'),

      shouldBe('A2', valueI(3)),
      shouldBe('A3', valueI(2)),

      exec(done)
    ]);
  });
  it('TYPE', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A2', 'Smith'),
      excel('A3', 'Formula'),
      excel('A4', '=TYPE(A2)'),
      excel('A5', '=TYPE("Mr. "&A2)'),
      excel('A6', '=TYPE(2+A2)'),
      excel('A7', '=(2+A2)'),
      excel('A8', '=TYPE({1,2;3,4})'),
      excel('B3', 'Description'),
      excel('B4', 'Returns the type of the value in A2. The Text type is indicated by 2.'),
      excel('B5', 'Returns the type of "Mr. Smith, which is Text.'),
      excel('B6', 'Returns the type of the formula in C6, which returns 16, the type for the error message  #VALUE!  The error message #VALUE! is shown in C7.'),
      excel('B7', 'The error value returned by the formula =(2+A2), which is used in C2.'),
      excel('B8', 'Returns the type of an array constant, which is 64.'),
      excel('C3', 'Result'),

      shouldBe('A4', valueI(2)),
      shouldBe('A5', valueI(2)),
      shouldBe('A6', valueI(16)),
      shouldBe('A7', valueS('#VALUE!')),
      shouldBe('A8', valueI(64)),

      exec(done)
    ]);
  });
  it('AND', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=AND(TRUE, TRUE)'),
      excel('A3', '=AND(TRUE, FALSE)'),
      excel('A4', '=AND(2+2=4, 2+3=5)'),
      excel('B1', 'Description'),
      excel('B2', 'All arguments are TRUE'),
      excel('B3', 'One argument is FALSE'),
      excel('B4', 'All arguments evaluate to TRUE'),
      excel('C1', 'Result'),

      shouldBe('A2', valueB(true)),
      shouldBe('A3', valueB(false)),
      shouldBe('A4', valueB(true)),

      exec(done)
    ]);
  });
  it('AND', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A2', '50'),
      excel('A3', '104'),
      excel('A4', 'Formula'),
      excel('A5', '=AND(1<A2, A2<100)'),
      excel('A6', '=IF(AND(1<A3, A3<100), A3, "The value is out of range.")'),
      excel('A7', '=IF(AND(1<A2, A2<100), A2, "The value is out of range.")'),
      excel('B4', 'Description'),
      excel('B5', 'Displays TRUE if the number in cell A2 is between 1 and 100. Otherwise, it displays FALSE.'),
      excel('B6', 'Displays the number in cell A3, if it is between 1 and 100. Otherwise, it displays the message "The value is out of range."'),
      excel('B7', 'Displays the number in cell A2, if it is between 1 and 100. Otherwise, it displays a message.'),
      excel('C4', 'Result'),

      shouldBe('A5', valueB(true)),
      shouldBe('A6', valueS('The value is out of range.')),
      shouldBe('A7', valueI(50)),

      exec(done)
    ]);
  });
  it('IFERROR', (done) => {
    _do([
      excel('A1', 'Quota'),
      excel('A2', '210'),
      excel('A3', '55'),
      excel('A5', 'Formula'),
      excel('A6', '=IFERROR(A2/B2, "Error in calculation")'),
      excel('A7', '=IFERROR(A3/B3, "Error in calculation")'),
      excel('A8', '=IFERROR(A4/B4, "Error in calculation")'),
      excel('B1', 'Units Sold'),
      excel('B2', '35'),
      excel('B3', '0'),
      excel('B4', '23'),
      excel('B5', 'Description'),
      excel('B6', 'Checks for an error in the formula in the first argument (divide 210 by 35), finds no error, and then returns the results of the formula'),
      excel('B7', 'Checks for an error in the formula in the first argument (divide 55 by 0), finds a division by 0 error, and then returns value_if_error'),
      excel('B8', 'Checks for an error in the formula in the first argument (divide "" by 23), finds no error, and then returns the results of the formula.'),
      excel('C5', 'Result'),

      shouldBe('A6', valueI(6)),
      shouldBe('A7', valueS('Error in calculation')),
      shouldBe('A8', valueI(0)),

      exec(done)
    ]);
  });
  it('IFERROR', (done) => {
    _do([
      excel('A1', 'Quota'),
      excel('A2', '210'),
      excel('A3', '55'),
      excel('A5', 'Formula'),
      excel('A6', '=C2'),
      excel('A7', '=C3'),
      excel('A8', '=C4'),
      excel('B1', 'Units Sold'),
      excel('B10', 'Note: The formula in the example must be entered as an array formula. After copying the example to a blank worksheet, select the range C2:C4, press F2, and then press CTRL+SHIFT+ENTER.'),
      excel('B2', '35'),
      excel('B3', '0'),
      excel('B4', '23'),
      excel('B5', 'Description'),
      excel('B6', 'Checks for an error in the formula in the first argument in the first element of the array (A2/B2 or divide 210 by 35), finds no error, and then returns the result of the formula'),
      excel('B7', 'Checks for an error in the formula in the first argument in the second element of the array (A3/B3 or divide 55 by 0), finds a division by 0 error, and then returns value_if_error'),
      excel('B8', 'Checks for an error in the formula in the first argument in the third element of the array (A4/B4 or divide "" by 23), finds no error, and then returns the result of the formula'),
      excel('C1', 'Ratio'),
      excel('C2', '6'),
      excel('C3', 'Error in calculation'),
      excel('C4', '0'),
      excel('C5', 'Result'),

      shouldBe('A6', valueI(6)),
      shouldBe('A7', valueS('Error in calculation')),
      shouldBe('A8', valueI(0)),

      exec(done)
    ]);
  });
  it('IFNA', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A10', 'Cleveland'),
      excel('A2', '=IFNA(VLOOKUP("Seattle",$A$5:$B$10,0),"Not found")'),
      excel('A4', 'Region ID'),
      excel('A5', 'Atlanta'),
      excel('A6', 'Portland'),
      excel('A7', 'Chicago'),
      excel('A8', 'Los Angeles'),
      excel('A9', 'Boise'),
      excel('B1', 'Description'),
      excel('B10', '275'),
      excel('B2', 'IFNA tests the result of the VLOOKUP function. Because Seattle is not found in the lookup range, VLOOKUP returns the #N/A error value. IFNA returns the string "Not found" in the cell instead of the standard #N/A error value.'),
      excel('B4', 'City'),
      excel('B5', '105'),
      excel('B6', '142'),
      excel('B7', '175'),
      excel('B8', '251'),
      excel('B9', '266'),
      excel('C1', 'Result'),

      shouldBe('A2', valueS('Not found')),

      exec(done)
    ]);
  });
  it('NOT', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=NOT(FALSE)'),
      excel('A3', '=NOT(1+1=2)'),
      excel('B1', 'Description'),
      excel('B2', 'Reverses FALSE'),
      excel('B3', 'Reverses an equation that evaluates to TRUE'),
      excel('C1', 'Result'),

      shouldBe('A2', valueB(true)),
      shouldBe('A3', valueB(false)),

      exec(done)
    ]);
  });
  it('OR', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=OR(TRUE)'),
      excel('A3', '=OR(1+1=1,2+2=5)'),
      excel('A4', '=OR(TRUE,FALSE,TRUE)'),
      excel('A5', '=IF(OR(1+1=1,2+2=5,5+5=10),"answer if true","answer if false")'),
      excel('B1', 'Description'),
      excel('B2', 'One argument is TRUE'),
      excel('B3', 'All arguments evaluate to FALSE'),
      excel('B4', 'At least one argument is TRUE'),
      excel('B5', 'One of the OR arguments are true'),
      excel('C1', 'Result'),

      shouldBe('A2', valueB(true)),
      shouldBe('A3', valueB(false)),
      shouldBe('A4', valueB(true)),
      shouldBe('A5', valueS('answer if true')),

      exec(done)
    ]);
  });
  it('XOR', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=XOR(3>0,2<9)'),
      excel('A3', '=XOR(3>12,4>6)'),
      excel('B1', 'Description'),
      excel('B2', 'Because one of the two tests evaluates to True, TRUE is returned.'),
      excel('B3', 'Because all test results evaluate to False, FALSE is returned. At least one of the test results must evaluate to True to return TRUE.'),
      excel('C1', 'Result'),

      shouldBe('A2', valueB(true)),
      shouldBe('A3', valueB(false)),

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
  it('AREAS', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=AREAS(B2:D4)'),
      excel('A3', '=AREAS((B2:D4,E5,F6:I9))'),
      excel('A4', '=AREAS(B2:D4 B2)'),
      excel('B1', 'Description'),
      excel('B2', 'Number of areas in the range'),
      excel('B3', 'Number of areas in the range'),
      excel('B4', 'Number of areas in the range'),
      excel('C1', 'Result'),

      shouldBe('A2', valueI(1)),
      shouldBe('A3', valueI(3)),
      shouldBe('A4', valueI(1)),

      exec(done)
    ]);
  });
  it('CHOOSE', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A2', '1st'),
      excel('A3', '2nd'),
      excel('A4', '3rd'),
      excel('A5', 'Finished'),
      excel('A6', 'Formula'),
      excel('A7', '=CHOOSE(2,A2,A3,A4,A5)'),
      excel('A8', '=CHOOSE(4,B2,B3,B4,B5)'),
      excel('A9', '=CHOOSE(3,"Wide",115,"world",8)'),
      excel('B2', 'Nails'),
      excel('B3', 'Screws'),
      excel('B4', 'Nuts'),
      excel('B5', 'Bolts'),
      excel('B6', 'Description'),
      excel('B7', 'Value of the second list argument (value of cell A3)'),
      excel('B8', 'Value of the fourth list argument (value of cell B5)'),
      excel('B9', 'Value of the third list argument'),
      excel('C6', 'Result'),

      shouldBe('A7', valueS('2nd')),
      shouldBe('A8', valueS('Bolts')),
      shouldBe('A9', valueS('world')),

      exec(done)
    ]);
  });
  it('CHOOSE', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A2', '23'),
      excel('A3', '45'),
      excel('A4', '12'),
      excel('A5', '10'),
      excel('A6', 'Formula'),
      excel('A7', '=SUM(A2:CHOOSE(2,A3,A4,A5))'),
      excel('B6', 'Description (Result)'),
      excel('B7', 'Sums the range A2:A4. The CHOOSE function returns A4 as the second part of the range for the SUM function.'),
      excel('C6', 'Result'),

      shouldBe('A7', valueI(80)),

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

      shouldBe('A2', valueI(3)),
      shouldBe('A3', valueI(2)),

      exec(done)
    ]);
  });
  it('COLUMNS', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=COLUMNS(C1:E4)'),
      excel('A3', '=COLUMNS({1,2,3;4,5,6})'),
      excel('B1', 'Description'),
      excel('B2', 'Number of columns in the reference C1:E4.'),
      excel('B3', 'Number of columns in the array constant {1,2,3;4,5,6}. There are two 3-column rows, containing 1,2, and 3 in the first row and 4,5, and 6 in the second row.'),
      excel('C1', 'Result'),

      shouldBe('A2', valueI(3)),
      shouldBe('A3', valueI(3)),

      exec(done)
    ]);
  });
  it('FORMULATEXT', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=TODAY()'),
      excel('B1', 'Description'),
      excel('B2', 'The formula in C2 returns the formula it finds in cell A2 as a text string so that you can easily inspect its structure. The formula entered in A2 is =TODAY(), and will return the current day in A2. The formula =TODAY() should appear as text in C2.'),
      excel('C1', 'Result'),

      shouldBe('A2', valueS('=FORMULATEXT(A2)')),

      exec(done)
    ]);
  });
  it('GETPIVOTDATA', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', 'GETPIVOTDATA("Sales", $A$4)'),
      excel('A3', 'GETPIVOTDATA("Sum of Sales", $A$4)'),
      excel('A4', 'GETPIVOTDATA("Sales", $A$4, "Month", "March")'),
      excel('A5', 'GETPIVOTDATA("Sales", $A$4, "Month", "March", "Product", "Produce", "Salesperson", "Buchanan")'),
      excel('A6', 'GETPIVOTDATA("Sales", $A$4, "Region", "South")'),
      excel('A7', 'GETPIVOTDATA("Sales", $A$4, "Product", "Beverages", "Salesperson", "Davolio")'),
      excel('B1', 'Result'),
      excel('B2', 'Returns the grand total of the Sales field, $49,325.'),
      excel('B3', 'Also returns the grand total of the Sales field, $49,325; the field name can be entered exactly as it looks on the sheet, or as its root (without "Sum of," "Count of," and so on).'),
      excel('B4', 'Returns the grand total for March, $30,337.'),
      excel('B5', 'Returns $10,201.'),
      excel('B6', 'Returns #REF! error value because the South region data is not visible.'),
      excel('B7', 'Returns #REF! error value because there is no total value of beverage sales for Davolio.'),


      exec(done)
    ]);
  });
  it('HLOOKUP', (done) => {
    _do([
      excel('A1', 'Axles'),
      excel('A10', '=HLOOKUP(3, {1,2,3;"a","b","c";"d","e","f"}, 2, TRUE)'),
      excel('A2', '4'),
      excel('A3', '5'),
      excel('A4', '6'),
      excel('A5', 'Formula'),
      excel('A6', '=HLOOKUP("Axles", A1:C4, 2, TRUE)'),
      excel('A7', '=HLOOKUP("Bearings", A1:C4, 3, FALSE)'),
      excel('A8', '=HLOOKUP("B", A1:C4, 3, TRUE)'),
      excel('A9', '=HLOOKUP("Bolts", A1:C4, 4)'),
      excel('B1', 'Bearings'),
      excel('B10', 'Looks up the number 3 in the three-row array constant, and returns the value from row 2 in the same (in this case, third) column. There are three rows of values in the array constant, each row separated by a semicolon (;). Because "c" is found in row 2 and in the same column as 3, "c" is returned.'),
      excel('B2', '4'),
      excel('B3', '7'),
      excel('B4', '8'),
      excel('B5', 'Description'),
      excel('B6', 'Looks up "Axles" in row 1, and returns the value from row 2 that\'s in the same column (column A).'),
      excel('B7', 'Looks up "Bearings" in row 1, and returns the value from row 3 that\'s in the same column (column B).'),
      excel('B8', 'Looks up "B" in row 1, and returns the value from row 3 that\'s in the same column. Because an exact match for "B" is not found, the largest value in row 1 that is less than "B" is used: "Axles," in column A.'),
      excel('B9', 'Looks up "Bolts" in row 1, and returns the value from row 4 that\'s in the same column (column C).'),
      excel('C1', 'Bolts'),
      excel('C2', '9'),
      excel('C3', '10'),
      excel('C4', '11'),
      excel('C5', 'Result'),

      shouldBe('A10', valueS('c')),
      shouldBe('A6', valueI(4)),
      shouldBe('A7', valueI(7)),
      shouldBe('A8', valueI(5)),
      shouldBe('A9', valueI(11)),

      exec(done)
    ]);
  });
  it('HYPERLINK', (done) => {
    _do([
      excel('A1', 'Example'),
      excel('A10', '=HYPERLINK($Z$1)'),
      excel('A2', '=HYPERLINK("http://example.microsoft.com/report/budget report.xlsx", "Click for report")'),
      excel('A3', '=HYPERLINK("[http://example.microsoft.com/report/budget report.xlsx]Annual!F10", D1)'),
      excel('A4', '=HYPERLINK("[http://example.microsoft.com/report/budget report.xlsx]\'First Quarter\'!DeptTotal", "Click to see First Quarter Department Total")'),
      excel('A5', '=HYPERLINK("http://example.microsoft.com/Annual Report.docx]QrtlyProfits", "Quarterly Profit Report")'),
      excel('A6', '=HYPERLINK("\\FINANCE\Statements\1stqtr.xlsx", D5)'),
      excel('A7', '=HYPERLINK("D:\FINANCE\1stqtr.xlsx", H10)'),
      excel('A8', '=HYPERLINK("[C:\My Documents\Mybook.xlsx]Totals")'),
      excel('A9', '=HYPERLINK("[Budget.xlsx]E56", E56)'),
      excel('B1', 'Result'),

      shouldBe('A10', valueS('To quickly update all formulas in a worksheet that use a HYPERLINK function with the same arguments, you can place the link target in another cell on the same or another worksheet, and then use an absolute reference to that cell as the link_location in the HYPERLINK formulas. Changes that you make to the link target are immediately reflected in the HYPERLINK formulas.')),
      shouldBe('A2', valueS('Opens a workbook saved at http://example.microsoft.com/report. The cell displays "Click for report" as its jump text.')),
      shouldBe('A3', valueS('Creates a hyperlink to cell F10 on the Annual worksheet in the workbook saved at http://example.microsoft.com/report. The cell on the worksheet that contains the hyperlink displays the contents of cell D1 as its jump text.')),
      shouldBe('A4', valueS('Creates a hyperlink to the range named DeptTotal on the First Quarter worksheet in the workbook saved at http://example.microsoft.com/report. The cell on the worksheet that contains the hyperlink displays "Click to see First Quarter Department Total" as its jump text.')),
      shouldBe('A5', valueS('To create a hyperlink to a specific location in a Word file, you use a bookmark to define the location you want to jump to in the file. This example creates a hyperlink to the bookmark QrtlyProfits in the file Annual Report.doc saved at http://example.microsoft.com.')),
      shouldBe('A6', valueS('Displays the contents of cell D5 as the jump text in the cell and opens the workbook saved on the FINANCE server in the Statements share. This example uses a UNC path.')),
      shouldBe('A7', valueS('Opens the workbook 1stqtr.xlsx that is stored in the Finance directory on drive D, and displays the numeric value that is stored in cell H10.')),
      shouldBe('A8', valueS('Creates a hyperlink to the Totals area in another (external) workbook, Mybook.xlsx.')),
      shouldBe('A9', valueS('To jump to a different sheet in the same workbook, include the name of the sheet, followed by an exclamation point (!), in the link. In the previous example, to create a link to cell E56 on the September sheet, include September! in the link.')),

      exec(done)
    ]);
  });
  it('INDEX', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A2', 'Apples'),
      excel('A3', 'Bananas'),
      excel('A4', 'Formula'),
      excel('A5', '\'=INDEX(A2:B3,2,2)'),
      excel('A6', '\'=INDEX(A2:B3,2,1)'),
      excel('B1', 'Data'),
      excel('B2', 'Lemons'),
      excel('B3', 'Pears'),
      excel('B4', 'Description'),
      excel('B5', 'Value at the intersection of the second row and second column in the range A2:B3.'),
      excel('B6', 'Value at the intersection of the second row and first column in the range A2:B3.'),
      excel('C4', 'Result'),

      shouldBe('A5', valueS('=INDEX(A2:B3,2,2)')),
      shouldBe('A6', valueS('=INDEX(A2:B3,2,1)')),

      exec(done)
    ]);
  });
  it('INDEX', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '\'=INDEX({1,2;3,4},0,2)'),
      excel('B1', 'Description'),
      excel('B2', 'Value found in the first row, second column in the array. The array contains 1 and 2 in the first row and 3 and 4 in the second row.'),
      excel('B3', 'Value found in the second row, second column in the array (same array as above).'),
      excel('C1', 'Result'),

      shouldBe('A2', valueS('=INDEX({1,2;3,4},0,2)')),
      shouldBe('A3', valueS('=INDEX({1,2;3,4},0,2)')),

      exec(done)
    ]);
  });
  it('INDEX', (done) => {
    _do([
      excel('A1', 'Fruit'),
      excel('A10', 'Peanuts'),
      excel('A11', 'Walnuts'),
      excel('A12', 'Formula'),
      excel('A13', '=INDEX(A2:C6, 2, 3)'),
      excel('A14', '=INDEX((A1:C6, A8:C11), 2, 2, 2)'),
      excel('A15', '=SUM(INDEX(A1:C11, 0, 3, 1))'),
      excel('A16', '=SUM(B2:INDEX(A2:C6, 5, 2))'),
      excel('A2', 'Apples'),
      excel('A3', 'Bananas'),
      excel('A4', 'Lemons'),
      excel('A5', 'Oranges'),
      excel('A6', 'Pears'),
      excel('A8', 'Almonds'),
      excel('A9', 'Cashews'),
      excel('B1', 'Price'),
      excel('B10', '$1.25'),
      excel('B11', '$1.75'),
      excel('B12', 'Description'),
      excel('B13', 'The intersection of the second row and third column in the range A2:C6, which is the contents of cell C3.'),
      excel('B14', 'The intersection of the second row and second column in the second area of A8:C11, which is the contents of cell B9.'),
      excel('B15', 'The sum of the third column in the first area of the range A1:C11, which is the sum of C1:C6.'),
      excel('B16', 'The sum of the range starting at B2, and ending at the intersection of the fifth row and the second column of the range A2:A6, which is the sum of B2:B6.'),
      excel('B2', '$0.69'),
      excel('B3', '$0.34'),
      excel('B4', '$0.55'),
      excel('B5', '$0.25'),
      excel('B6', '$0.59'),
      excel('B8', '$2.80'),
      excel('B9', '$3.55'),
      excel('C1', 'Count'),
      excel('C10', '20'),
      excel('C11', '12'),
      excel('C12', 'Result'),
      excel('C2', '40'),
      excel('C3', '38'),
      excel('C4', '15'),
      excel('C5', '25'),
      excel('C6', '40'),
      excel('C8', '10'),
      excel('C9', '16'),

      shouldBe('A13', valueI(38)),
      shouldBe('A14', valueD(3.55)),
      shouldBe('A15', valueI(216)),
      shouldBe('A16', valueD(2.42)),

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
  it('MATCH', (done) => {
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
      shouldBe('A4', valueS('#REF!')),

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
  it('ROWS', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=ROWS(C1:E4)'),
      excel('A3', '=ROWS({1,2,3;4,5,6})'),
      excel('B1', 'Description'),
      excel('B2', 'Number of rows in the reference'),
      excel('B3', 'Number of rows in the array constant'),
      excel('C1', 'Result'),

      shouldBe('A2', valueI(4)),
      shouldBe('A3', valueI(2)),

      exec(done)
    ]);
  });
  it('RTD', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=RTD("mycomaddin.progid",,"Server_name","Price")'),
      excel('A4', 'Note'),
      excel('A5', 'The RTD COM automation add-in must be created and registered on a local computer. If you haven\'t installed a real-time data server, the RTD function returns the #NAME? error message in a cell when you try to use the RTD function.'),
      excel('B1', 'Description (Result)'),
      excel('B2', 'Real-time data retrieved from a program that supports COM automation.'),
      excel('C1', 'Result'),

      shouldBe('A2', valueS('#NAME?')),

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
  it('ACOS', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=ACOS(-0.5)'),
      excel('A3', '=ACOS(-0.5)*180/PI()'),
      excel('A4', '=DEGREES(ACOS(-0.5))'),
      excel('B1', 'Description'),
      excel('B2', 'Arccosine of -0.5 in radians, 2*pi/3'),
      excel('B3', 'Arccosine of -0.5 in degrees'),
      excel('B4', 'Arccosine of -0.5 in degrees'),
      excel('C1', 'Result'),

      shouldBe('A2', valueD(2.094395102)),
      shouldBe('A3', valueI(120)),
      shouldBe('A4', valueI(120)),

      exec(done)
    ]);
  });
  it('ACOSH', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=ACOSH(1)'),
      excel('A3', '=ACOSH(10)'),
      excel('B1', 'Description'),
      excel('B2', 'Inverse hyperbolic cosine of 1'),
      excel('B3', 'Inverse hyperbolic cosine of 10'),
      excel('C1', 'Result'),

      shouldBe('A2', valueI(0)),
      shouldBe('A3', valueD(2.9932228)),

      exec(done)
    ]);
  });
  it('ACOT', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=ACOT(2)'),
      excel('B1', 'Description'),
      excel('B2', 'Returns the arccotangent of 2, in radians (0.4636).'),
      excel('C1', 'Result'),

      shouldBe('A2', valueD(0.4636)),

      exec(done)
    ]);
  });
  it('ACOTH', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=ACOTH(6)'),
      excel('B1', 'Description'),
      excel('B2', 'Returns the hyperbolic arccotangent of 6 (0.168).'),
      excel('C1', 'Result'),

      shouldBe('A2', valueD(0.168)),

      exec(done)
    ]);
  });
  it('AGGREGATE', (done) => {
    _do([
      excel('A1', '#DIV/0!'),
      excel('A10', '53'),
      excel('A11', '34'),
      excel('A12', 'Formula'),
      excel('A13', '=AGGREGATE(4, 6, A1:A11)'),
      excel('A14', '=AGGREGATE(14, 6, A1:A11, 3)'),
      excel('A15', '=AGGREGATE(15, 6, A1:A11)'),
      excel('A16', '=AGGREGATE(12, 6, A1:A11, B1:B11)'),
      excel('A17', '=MAX(A1:A2)'),
      excel('A2', '72'),
      excel('A3', '30'),
      excel('A4', '#NUM!'),
      excel('A5', '31'),
      excel('A6', '96'),
      excel('A7', '32'),
      excel('A8', '81'),
      excel('A9', '33'),
      excel('B1', '82'),
      excel('B10', '91'),
      excel('B11', '89'),
      excel('B12', 'Description'),
      excel('B13', 'Calculates the maximum value while ignoring error values in the range'),
      excel('B14', 'Calculates the 3rd largest value while ignoring error values in the range'),
      excel('B15', 'Will return #VALUE! error. This is because AGGREGATE is expecting a second ref argument, since the function (SMALL) requires one.'),
      excel('B16', 'Calculates the median while ignoring error values in the range'),
      excel('B17', 'Will return error value, since there are error values in the evaluation range.'),
      excel('B2', '65'),
      excel('B3', '95'),
      excel('B4', '63'),
      excel('B5', '53'),
      excel('B6', '71'),
      excel('B7', '55'),
      excel('B8', '83'),
      excel('B9', '100'),
      excel('C12', 'Result'),

      shouldBe('A13', valueI(96)),
      shouldBe('A14', valueI(72)),
      shouldBe('A15', valueS('#VALUE!')),
      shouldBe('A16', valueI(68)),
      shouldBe('A17', valueS('#DIV/0!')),

      exec(done)
    ]);
  });
  it('ARABIC', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=ARABIC("LVII")'),
      excel('A4', '=ARABIC(A6)'),
      excel('A6', 'mcmxii'),
      excel('B1', 'Description'),
      excel('B2', 'Returns an Arabic number based on the Roman number LVII (57).'),
      excel('B4', 'Returns an Arabic number based on a Roman number in cell A6 (1912).'),
      excel('C1', 'Result'),

      shouldBe('A2', valueI(57)),
      shouldBe('A4', valueI(1912)),

      exec(done)
    ]);
  });
  it('ASIN', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=ASIN(-0.5)'),
      excel('A3', '=ASIN(-0.5)*180/PI()'),
      excel('A4', '=DEGREES(ASIN(-0.5))'),
      excel('B1', 'Description'),
      excel('B2', 'Arcsine of -0.5 in radians, -pi/6'),
      excel('B3', 'Arcsine of -0.5 in degrees'),
      excel('B4', 'Arcsine of -0.5 in degrees'),
      excel('C1', 'Result'),

      shouldBe('A2', valueD(-0.523598776)),
      shouldBe('A3', valueI(-30)),
      shouldBe('A4', valueI(-30)),

      exec(done)
    ]);
  });
  it('ASINH', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=ASINH(-2.5)'),
      excel('A3', '=ASINH(10)'),
      excel('B1', 'Description'),
      excel('B2', 'Inverse hyperbolic sine of -2.5'),
      excel('B3', 'Inverse hyperbolic sine of 10'),
      excel('C1', 'Result'),

      shouldBe('A2', valueD(-1.647231146)),
      shouldBe('A3', valueD(2.99822295)),

      exec(done)
    ]);
  });
  it('ATAN', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=ATAN(1)'),
      excel('A3', '=ATAN(1)*180/PI()'),
      excel('A4', '=DEGREES(ATAN(1))'),
      excel('B1', 'Description'),
      excel('B2', 'Arctangent of 1 in radians, pi/4'),
      excel('B3', 'Arctangent of 1 in degrees'),
      excel('B4', 'Arctangent of 1 in degrees'),
      excel('C1', 'Result'),

      shouldBe('A2', valueD(0.785398163)),
      shouldBe('A3', valueI(45)),
      shouldBe('A4', valueI(45)),

      exec(done)
    ]);
  });
  it('ATAN2', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=ATAN2(1, 1)'),
      excel('A3', '=ATAN2(-1, -1)'),
      excel('A4', '=ATAN2(-1, -1)*180/PI()'),
      excel('A5', '=DEGREES(ATAN2(-1, -1))'),
      excel('B1', 'Description'),
      excel('B2', 'Arctangent of the point 1,1 in radians, pi/4'),
      excel('B3', 'Arctangent of the point -1,-1 in radians, -3*pi/4'),
      excel('B4', 'Arctangent of the point 1,1 in degrees'),
      excel('B5', 'Arctangent of the point 1,1 in degrees'),
      excel('C1', 'Result'),

      shouldBe('A2', valueD(0.785398163)),
      shouldBe('A3', valueD(-2.35619449)),
      shouldBe('A4', valueI(-135)),
      shouldBe('A5', valueI(-135)),

      exec(done)
    ]);
  });
  it('ATANH', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=ATANH(0.76159416)'),
      excel('A3', '=ATANH(-0.1)'),
      excel('B1', 'Description'),
      excel('B2', 'Inverse hyperbolic tangent of 0.76159416'),
      excel('C1', 'Result'),

      shouldBe('A2', valueD(1.00000001)),
      shouldBe('A3', valueD(-0.100335348)),

      exec(done)
    ]);
  });
  it('BASE', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=BASE(7,2)'),
      excel('A4', '=BASE(100,16)'),
      excel('A6', '=BASE(15,2,10)'),
      excel('B1', 'Description'),
      excel('B2', 'Converts the decimal number 7 to base 2 (binary). Result is 111.'),
      excel('B4', 'Converts the decimal number 100 to base 16 (hexadecimal). The result is 64.'),
      excel('B6', 'Converts the decimal number 15 to base 2 (binary), with a minimum length of 10. The result is 0000001111, which is 1111 with 6 leading zeros to make the string 10 characters long.'),
      excel('C1', 'Result'),

      shouldBe('A2', valueS('111')),
      shouldBe('A4', valueS('64')),
      shouldBe('A6', valueS('0000001111')),

      exec(done)
    ]);
  });
  it('CEILING', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=CEILING(2.5, 1)'),
      excel('A3', '=CEILING(-2.5, -2)'),
      excel('A4', '=CEILING(-2.5, 2)'),
      excel('A5', '=CEILING(1.5, 0.1)'),
      excel('A6', '=CEILING(0.234, 0.01)'),
      excel('B1', 'Description'),
      excel('B2', 'Rounds 2.5 up to nearest multiple of 1'),
      excel('B3', 'Rounds -2.5 up to nearest multiple of -2'),
      excel('B4', 'Rounds -2.5 up to nearest multiple of 2'),
      excel('B5', 'Rounds 1.5 up to the nearest multiple of 0.1'),
      excel('B6', 'Rounds 0.234 up to the nearest multiple of 0.01'),
      excel('C1', 'Result'),

      shouldBe('A2', valueI(3)),
      shouldBe('A3', valueI(-4)),
      shouldBe('A4', valueI(-2)),
      shouldBe('A5', valueD(1.5)),
      shouldBe('A6', valueD(0.24)),

      exec(done)
    ]);
  });
  it('CEILINGMATH', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=CEILING.MATH(24.3,5)'),
      excel('A4', '=CEILING.MATH(6.7)'),
      excel('A6', '=CEILING.MATH(-8.1,2)'),
      excel('A8', '=CEILING.MATH(-5.5,2,-1)'),
      excel('B1', 'Description'),
      excel('B2', 'Rounds 24.3 up to the nearest integer that is a multiple of 5 (25).'),
      excel('B4', 'Rounds 6.7 up to the nearest integer (7).'),
      excel('B6', 'Rounds -8.1 up (toward 0) to the nearest integer that is a multiple of 2 (-8).'),
      excel('B8', 'Rounds -5.5 down (away from 0) to the nearest integer that is a multiple of 2 with a mode of -1, which reverses rounding direction (-6).'),
      excel('C1', 'Result'),

      shouldBe('A2', valueI(25)),
      shouldBe('A4', valueI(7)),
      shouldBe('A6', valueI(-8)),
      shouldBe('A8', valueI(-6)),

      exec(done)
    ]);
  });
  it('CEILINGPRECISE', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=CEILING.PRECISE(4.3)'),
      excel('A3', '=CEILING.PRECISE(-4.3)'),
      excel('A4', '=CEILING.PRECISE(4.3, 2)'),
      excel('A5', '=CEILING.PRECISE(4.3,-2)'),
      excel('A6', '=CEILING.PRECISE(-4.3,2)'),
      excel('A7', '=CEILING.PRECISE(-4.3,-2)'),
      excel('B1', 'Description'),
      excel('B2', 'Rounds 4.3 up to the nearest multiple of 1.'),
      excel('B3', 'Rounds -4.3 up to the nearest multiple of 1. Rounds toward 0 because the number is negative.'),
      excel('B4', 'Rounds 4.3 up to the nearest multiple of 2.'),
      excel('B5', 'Rounds 4.3 up to the nearest multiple of -2.'),
      excel('B6', 'Rounds -4.3 up to the nearest multiple of 2. Rounds toward 0 because the number is negative.'),
      excel('B7', 'Rounds -4.3 up to the nearest multiple of -2.'),
      excel('C1', 'Result'),

      shouldBe('A2', valueI(5)),
      shouldBe('A3', valueI(-4)),
      shouldBe('A4', valueI(6)),
      shouldBe('A5', valueI(6)),
      shouldBe('A6', valueI(-4)),
      shouldBe('A7', valueI(-4)),

      exec(done)
    ]);
  });
  it('COMBIN', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=COMBIN(8,2)'),
      excel('B1', 'Description'),
      excel('B2', 'Possible two-person teams that can be formed from 8 candidates.'),
      excel('C1', 'Result'),

      shouldBe('A2', valueI(28)),

      exec(done)
    ]);
  });
  it('COMBINA', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=COMBINA(4,3)'),
      excel('A3', '=COMBINA(10,3)'),
      excel('B1', 'Description'),
      excel('B2', 'Returns the number of combinations (with repetitions) for 4 and 3.'),
      excel('B3', 'Returns the number of combinations (with repetitions) for 10 and 3.'),
      excel('C1', 'Result'),

      shouldBe('A2', valueI(20)),
      shouldBe('A3', valueI(220)),

      exec(done)
    ]);
  });
  it('COS', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=COS(1.047)'),
      excel('A3', '=COS(60*PI()/180)'),
      excel('A4', '=COS(RADIANS(60))'),
      excel('B1', 'Description'),
      excel('B2', 'Cosine of 1.047 radians'),
      excel('B3', 'Cosine of 60 degrees'),
      excel('B4', 'Cosine of 60 degrees'),
      excel('C1', 'Result'),

      shouldBe('A2', valueD(0.5001711)),
      shouldBe('A3', valueD(0.5)),
      shouldBe('A4', valueD(0.5)),

      exec(done)
    ]);
  });
  it('COSH', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=COSH(4)'),
      excel('A3', '=COSH(EXP(1))'),
      excel('B1', 'Description'),
      excel('B2', 'Hyperbolic cosine of 4'),
      excel('B3', 'Hyperbolic cosine of the base of the natural logarithm.'),
      excel('C1', 'Result'),

      shouldBe('A2', valueD(27.308233)),
      shouldBe('A3', valueD(7.6101251)),

      exec(done)
    ]);
  });
  it('COT', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=COT(30)'),
      excel('A3', '=COT(A5)'),
      excel('A5', '45'),
      excel('B1', 'Description'),
      excel('B2', 'Returns the cotangent of 30 (-0.156).'),
      excel('B5', 'Returns the cotangent of the value in cell A5, 45 (0.617).'),
      excel('C1', 'Result'),
      excel('C5', '0.617'),

      shouldBe('A2', valueD(-0.156)),

      exec(done)
    ]);
  });
  it('COTH', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=COTH(2)'),
      excel('B1', 'Description'),
      excel('B2', 'Returns the hyperbolic cotangent of 2 (1.037).'),
      excel('C1', 'Result'),

      shouldBe('A2', valueD(1.037)),

      exec(done)
    ]);
  });
  it('CSC', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=CSC(15)'),
      excel('B1', 'Description'),
      excel('B2', 'Returns the cosecant of 15 (1.538).'),
      excel('C1', 'Result'),

      shouldBe('A2', valueD(1.538)),

      exec(done)
    ]);
  });
  it('CSCH', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=CSCH(1.5)'),
      excel('B1', 'Description'),
      excel('B2', 'Returns the hyperbolic cosecant of 1.5 (0.4696).'),
      excel('C1', 'Result'),

      shouldBe('A2', valueD(0.4696)),

      exec(done)
    ]);
  });
  it('DECIMAL', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '\'=DECIMAL("FF",16)'),
      excel('A5', '\'=DECIMAL(111,2)'),
      excel('A9', '\'=DECIMAL("zap",36)'),
      excel('B1', 'Description'),
      excel('B2', 'Converts the hexadecimal (base 16) value FF to its equivalent decimal (base 10) value (255).'),
      excel('B3', 'The HEX2DEC function in cell C3 verifies this result.'),
      excel('B5', 'Converts the binary (base 2) value 111 to its equivalent decimal (base 10) value (7).'),
      excel('B6', 'The BIN2DEC function in cell C6 verifies this result.'),
      excel('B9', 'Converts the value "zap" in base 36 to its equivalent decimal value (45745).'),
      excel('C1', 'Result'),
      excel('D1', 'How it works'),
      excel('D10', 'Formula'),
      excel('D11', '=(35*(36^2))+(10*(36^1))+(25*(36^0))'),
      excel('D2', '"F" is in position 15 in the base 16 number system. Because all number systems start with 0, the 16th character in hexadecimal will be in the 15th position. The formula below shows how it is converted to decimal:'),
      excel('D3', 'Formula'),
      excel('D4', '=(15*(16^1))+(15*(16^0))'),
      excel('D5', '"1" is in position 1 in the base 2 number system. The formula below shows how it is converted to decimal:'),
      excel('D6', 'Formula'),
      excel('D7', '=(1*(2^2))+(1*(2^1))+(1*(2^0))'),
      excel('D9', '"z" is in position 35, "a" is in position 10, and "p" is in position 25. The formula below shows how it is converted to decimal.'),

      shouldBe('A2', valueS('=DECIMAL("FF",16)')),
      shouldBe('A3', valueS('=HEX2DEC("ff")')),
      shouldBe('A5', valueS('=DECIMAL(111,2)')),
      shouldBe('A6', valueS('=BIN2DEC(111)')),
      shouldBe('A9', valueS('=DECIMAL("zap",36)')),

      exec(done)
    ]);
  });
  it('DEGREES', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=DEGREES(PI())'),
      excel('B1', 'Description'),
      excel('B2', 'Degrees of pi radians'),
      excel('C1', 'Result'),

      shouldBe('A2', valueI(180)),

      exec(done)
    ]);
  });
  it('EVEN', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=EVEN(1.5)'),
      excel('A3', '=EVEN(3)'),
      excel('A4', '=EVEN(2)'),
      excel('A5', '=EVEN(-1)'),
      excel('B1', 'Description'),
      excel('B2', 'Rounds 1.5 to the nearest even integer'),
      excel('B3', 'Rounds 3 to the nearest even integer'),
      excel('B4', 'Rounds 2 to the nearest even integer'),
      excel('B5', 'Rounds -1 to the nearest even integer'),
      excel('C1', 'Result'),

      shouldBe('A2', valueI(2)),
      shouldBe('A3', valueI(4)),
      shouldBe('A4', valueI(2)),
      shouldBe('A5', valueI(-2)),

      exec(done)
    ]);
  });
  it('EXP', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=EXP(1)'),
      excel('A3', '=EXP(2)'),
      excel('B1', 'Description'),
      excel('B2', 'Approximate value of e'),
      excel('B3', 'Base of the natural logarithm e raised to the power of 2'),
      excel('C1', 'Result'),

      shouldBe('A2', valueD(2.71828183)),
      shouldBe('A3', valueD(7.3890561)),

      exec(done)
    ]);
  });
  it('FACT', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=FACT(5)'),
      excel('A3', '=FACT(1.9)'),
      excel('A4', '=FACT(0)'),
      excel('A5', '=FACT(-1)'),
      excel('A6', '=FACT(1)'),
      excel('B1', 'Description'),
      excel('B2', 'Factorial of 5, or 1*2*3*4*5'),
      excel('B3', 'Factorial of the integer of 1.9'),
      excel('B4', 'Factorial of 0'),
      excel('B5', 'Factorial of a negative number returns an error value'),
      excel('B6', 'Factorial of 1'),
      excel('C1', 'Result'),

      shouldBe('A2', valueI(120)),
      shouldBe('A3', valueI(1)),
      shouldBe('A4', valueI(1)),
      shouldBe('A5', valueS('#NUM!')),
      shouldBe('A6', valueI(1)),

      exec(done)
    ]);
  });
  it('FACTDOUBLE', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=FACTDOUBLE(6)'),
      excel('A3', '=FACTDOUBLE(7)'),
      excel('B1', 'Description'),
      excel('B2', 'Double factorial of 6. For 6, an even number, the double factorial is equivalent to 6*4*2; using this equation:n!! = n*(n-2)*(n-4)...(4)(2)'),
      excel('B3', 'Double factorial of 7. For 7, an odd number, the double factorial is equivalent to 7*5*3; using this equation:n!! = n*(n-2)*(n-4)...(3)(1)'),
      excel('C1', 'Result'),

      shouldBe('A2', valueI(48)),
      shouldBe('A3', valueI(105)),

      exec(done)
    ]);
  });
  it('FLOOR', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=FLOOR(3.7,2)'),
      excel('A3', '=FLOOR(-2.5,-2)'),
      excel('A4', '=FLOOR(2.5,-2)'),
      excel('A5', '=FLOOR(1.58,0.1)'),
      excel('A6', '=FLOOR(0.234,0.01)'),
      excel('B1', 'Description'),
      excel('B2', 'Rounds 3.7 down to nearest multiple of 2.'),
      excel('B3', 'Rounds -2.5 down to nearest multiple of -2.'),
      excel('B4', 'Returns an error value, because 2.5 and -2 have different signs.'),
      excel('B5', 'Rounds 1.58 down to the nearest multiple of 0.1.'),
      excel('B6', 'Rounds 0.234 down to the nearest multiple of 0.01.'),
      excel('C1', 'Result'),

      shouldBe('A2', valueI(2)),
      shouldBe('A3', valueI(-2)),
      shouldBe('A4', valueS('#NUM!')),
      shouldBe('A5', valueD(1.5)),
      shouldBe('A6', valueD(0.23)),

      exec(done)
    ]);
  });
  it('FLOORMATH', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=FLOOR.MATH(24.3,5)'),
      excel('A4', '=FLOOR.MATH(6.7)'),
      excel('A6', '=FLOOR.MATH(-8.1,2)'),
      excel('A8', '=FLOOR.MATH(-5.5,2,-1)'),
      excel('B1', 'Description (result)'),
      excel('B2', 'Rounds 24.3 down to the nearest integer that is a multiple of 5 (20).'),
      excel('B4', 'Rounds 6.7 down to the nearest integer (6).'),
      excel('B6', 'Rounds -8.1 down (away from 0) to the nearest integer that is a multiple of 2 (-10).'),
      excel('B8', 'Rounds -5.5 toward 0 to the nearest integer that is a multiple of 2, using a non-zero Mode, which reverses rounding direction (-4).'),
      excel('C1', 'Result'),

      shouldBe('A2', valueI(20)),
      shouldBe('A4', valueI(6)),
      shouldBe('A6', valueI(-10)),
      shouldBe('A8', valueI(-4)),

      exec(done)
    ]);
  });
  it('FLOORPRECISE', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=FLOOR.PRECISE(-3.2,-1)'),
      excel('A3', '=FLOOR.PRECISE(3.2, 1)'),
      excel('A4', '=FLOOR.PRECISE(-3.2, 1)'),
      excel('A5', '=FLOOR.PRECISE(3.2,-1)'),
      excel('A6', '=FLOOR.PRECISE(3.2)'),
      excel('B1', 'Description'),
      excel('B2', 'Rounds -3.2 down to the nearest multiple of -1'),
      excel('B3', 'Rounds 3.2 down to the nearest multiple of 1'),
      excel('B4', 'Rounds -3.2 down to the nearest multiple of 1'),
      excel('B5', 'rounds 3.2 down to the nearest multiple of -1'),
      excel('B6', 'Rounds 3.2 down to nearest multiple of 1'),
      excel('C1', 'Result'),

      shouldBe('A2', valueI(-4)),
      shouldBe('A3', valueI(3)),
      shouldBe('A4', valueI(-4)),
      shouldBe('A5', valueI(3)),
      shouldBe('A6', valueI(3)),

      exec(done)
    ]);
  });
  it('GCD', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=GCD(5, 2)'),
      excel('A3', '=GCD(24, 36)'),
      excel('A4', '=GCD(7, 1)'),
      excel('A5', '=GCD(5, 0)'),
      excel('B1', 'Description'),
      excel('B2', 'Greatest common divisor of 5 and 2'),
      excel('B3', 'Greatest common divisor of 24 and 36'),
      excel('B4', 'Greatest common divisor of 7 and 1'),
      excel('B5', 'Greatest common divisor of 5 and 0'),
      excel('C1', 'Result'),

      shouldBe('A2', valueI(1)),
      shouldBe('A3', valueI(12)),
      shouldBe('A4', valueI(1)),
      shouldBe('A5', valueI(5)),

      exec(done)
    ]);
  });
  it('INT', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A2', '19.5'),
      excel('A3', 'Formula'),
      excel('A4', '=INT(8.9)'),
      excel('A5', '=INT(-8.9)'),
      excel('A6', '=A2-INT(A2)'),
      excel('B3', 'Description'),
      excel('B4', 'Rounds 8.9 down'),
      excel('B5', 'Rounds -8.9 down. Rounding a negative number down rounds it away from 0.'),
      excel('B6', 'Returns the decimal part of a positive real number in cell A2'),
      excel('C3', 'Result'),

      shouldBe('A4', valueI(8)),
      shouldBe('A5', valueI(-9)),
      shouldBe('A6', valueD(0.5)),

      exec(done)
    ]);
  });
  it('ISOCEILING', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=ISO.CEILING(4.3)'),
      excel('A3', '=ISO.CEILING(-4.3)'),
      excel('A4', '=ISO.CEILING(4.3, 2)'),
      excel('A5', '=ISO.CEILING(4.3,-2)'),
      excel('A6', '=ISO.CEILING(-4.3,2)'),
      excel('A7', '=ISO.CEILING(-4.3,-2)'),
      excel('B1', 'Description'),
      excel('B2', 'Rounds 4.3 up to nearest multiple of 1'),
      excel('B3', 'Rounds -4.3 up to nearest multiple of 1'),
      excel('B4', 'Rounds 4.3 up to the nearest multiple of 2'),
      excel('B5', 'rounds 4.3 up to the nearest multiple of -2'),
      excel('B6', 'Rounds -4.3 up to the nearest multiple of 2'),
      excel('B7', 'Rounds -4.3 up to the nearest multiple of -2'),
      excel('C1', 'Result'),

      shouldBe('A2', valueI(5)),
      shouldBe('A3', valueI(-4)),
      shouldBe('A4', valueI(6)),
      shouldBe('A5', valueI(6)),
      shouldBe('A6', valueI(-4)),
      shouldBe('A7', valueI(-4)),

      exec(done)
    ]);
  });
  it('LCM', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=LCM(5, 2)'),
      excel('A3', '=LCM(24, 36)'),
      excel('B1', 'Description'),
      excel('B2', 'Least common multiple of 5 and 2'),
      excel('B3', 'Least common multiple of 24 and 36'),
      excel('C1', 'Result'),

      shouldBe('A2', valueI(10)),
      shouldBe('A3', valueI(72)),

      exec(done)
    ]);
  });
  it('LN', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=LN(86)'),
      excel('A3', '=LN(2.7182818)'),
      excel('A4', '=LN(EXP(3))'),
      excel('B1', 'Description'),
      excel('B2', 'Natural logarithm of 86'),
      excel('B3', 'Natural logarithm of the value of the constant e'),
      excel('B4', 'Natural logarithm of e raised to the power of 3'),
      excel('C1', 'Result'),

      shouldBe('A2', valueD(4.4543473)),
      shouldBe('A3', valueI(1)),
      shouldBe('A4', valueI(3)),

      exec(done)
    ]);
  });
  it('LOG', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=LOG(10)'),
      excel('A3', '=LOG(8, 2)'),
      excel('A4', '=LOG(86, 2.7182818)'),
      excel('B1', 'Description'),
      excel('B2', 'Logarithm of 10. Because the second argument (base) is omitted, it is assumed to be 10. The result, 1, is the power to which the base must be raised to equal 10.'),
      excel('B3', 'Logarithm of 8 with base 2. The result, 3, is the power to which the base must be raised to equal 8.'),
      excel('B4', 'Logarithm of 86 with base e (approximately 2.718). The result, 4.454, is the power to which the base must be raised to equal 86.'),
      excel('C1', 'Result'),

      shouldBe('A2', valueI(1)),
      shouldBe('A3', valueI(3)),
      shouldBe('A4', valueD(4.4543473)),

      exec(done)
    ]);
  });
  it('LOG10', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=LOG10(86)'),
      excel('A3', '=LOG10(10)'),
      excel('A4', '=LOG10(100000)'),
      excel('A5', '=LOG10(10^5)'),
      excel('B1', 'Description'),
      excel('B2', 'Base 10 logarithm of 86. The result, 1.9345, is the power to raise the base, 10, to equal 86.'),
      excel('B3', 'Base 10 logarithm of 10. This is the power 10 is raised to to equal 10.'),
      excel('B4', 'Base 10 logarithm of 1E+5. This is the power 10 is raised to to equal 100000 (1E+5).'),
      excel('B5', 'Base 10 logarithm of 10^5. Same as above; 1E+5 = 100000.'),
      excel('C1', 'Result'),

      shouldBe('A2', valueD(1.9345)),
      shouldBe('A3', valueI(1)),
      shouldBe('A4', valueI(5)),
      shouldBe('A5', valueI(5)),

      exec(done)
    ]);
  });
  it('MDETERM', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A10', '=MDETERM({1,3,8,5;1,3,6,1})'),
      excel('A2', '1'),
      excel('A3', '1'),
      excel('A4', '1'),
      excel('A5', '7'),
      excel('A6', 'Formula'),
      excel('A7', '=MDETERM(A2:D5)'),
      excel('A8', '=MDETERM({3,6,1;1,1,0;3,10,2})'),
      excel('A9', '=MDETERM({3,6;1,1})'),
      excel('B1', 'Data'),
      excel('B10', 'Returns an error because the array does not have an equal number of rows and columns.'),
      excel('B2', '3'),
      excel('B3', '3'),
      excel('B4', '1'),
      excel('B5', '3'),
      excel('B6', 'Description'),
      excel('B7', 'Determinant of the matrix above'),
      excel('B8', 'Determinant of the matrix as an array constant'),
      excel('B9', 'Determinant of the matrix in the array constant'),
      excel('C1', 'Data'),
      excel('C2', '8'),
      excel('C3', '6'),
      excel('C4', '1'),
      excel('C5', '10'),
      excel('C6', 'Result'),
      excel('D1', 'Data'),
      excel('D2', '5'),
      excel('D3', '1'),
      excel('D4', '0'),
      excel('D5', '2'),

      shouldBe('A10', valueS('#VALUE!')),
      shouldBe('A7', valueI(88)),
      shouldBe('A8', valueI(1)),
      shouldBe('A9', valueI(-3)),

      exec(done)
    ]);
  });
  it('MOD', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=MOD(3, 2)'),
      excel('A3', '=MOD(-3, 2)'),
      excel('A4', '=MOD(3, -2)'),
      excel('A5', '=MOD(-3, -2)'),
      excel('B1', 'Description'),
      excel('B2', 'Remainder of 3/2'),
      excel('B3', 'Remainder of -3/2. The sign is the same as divisor'),
      excel('B4', 'Remainder of 3/-2. The sign is the same as divisor'),
      excel('B5', 'Remainder of -3/-2. The sign is the same as divisor'),
      excel('C1', 'Result'),

      shouldBe('A2', valueI(1)),
      shouldBe('A3', valueI(1)),
      shouldBe('A4', valueI(-1)),
      shouldBe('A5', valueI(-1)),

      exec(done)
    ]);
  });
  it('MROUND', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=MROUND(10, 3)'),
      excel('A3', '=MROUND(-10, -3)'),
      excel('A4', '=MROUND(1.3, 0.2)'),
      excel('A5', '=MROUND(5, -2)'),
      excel('B1', 'Description'),
      excel('B2', 'Rounds 10 to the nearest multiple of 3.'),
      excel('B3', 'Rounds -10 to the nearest multiple of -3.'),
      excel('B4', 'Rounds 1.3 to the nearest multiple of 0.2.'),
      excel('B5', 'Returns the #NUM! error message because -2 and 5 have different signs.'),
      excel('C1', 'Result'),

      shouldBe('A2', valueI(9)),
      shouldBe('A3', valueI(-9)),
      shouldBe('A4', valueD(1.4)),
      shouldBe('A5', valueS('#NUM!')),

      exec(done)
    ]);
  });
  it('MULTINOMIAL', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=MULTINOMIAL(2, 3, 4)'),
      excel('B1', 'Description'),
      excel('B2', 'Ratio of the factorial of the sum of 2,3, and 4 (362880) to the product of the factorials of 2,3, and 4 (288).'),
      excel('C1', 'Result'),

      shouldBe('A2', valueI(1260)),

      exec(done)
    ]);
  });
  it('ODD', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=ODD(1.5)'),
      excel('A3', '=ODD(3)'),
      excel('A4', '=ODD(2)'),
      excel('A5', '=ODD(-1)'),
      excel('A6', '=ODD(-2)'),
      excel('B1', 'Description'),
      excel('B2', 'Rounds 1.5 up to the nearest odd integer.'),
      excel('B3', 'Rounds 3 up to the nearest odd integer.'),
      excel('B4', 'Rounds 2 up to the nearest odd integer.'),
      excel('B5', 'Rounds -1 up to the nearest odd integer.'),
      excel('B6', 'Rounds -2 up (away from 0) to the nearest odd integer.'),
      excel('C1', 'Result'),

      shouldBe('A2', valueI(3)),
      shouldBe('A3', valueI(3)),
      shouldBe('A4', valueI(3)),
      shouldBe('A5', valueI(-1)),
      shouldBe('A6', valueI(-3)),

      exec(done)
    ]);
  });
  it('PI', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A2', 'Radius'),
      excel('A3', '3'),
      excel('A4', 'Formula'),
      excel('A5', '=PI()'),
      excel('A6', '=PI()/2'),
      excel('A7', '=PI()*(A3^2)'),
      excel('B4', 'Description'),
      excel('B5', 'Returns pi.'),
      excel('B6', 'Returns pi divided by 2.'),
      excel('B7', 'Area of a circle with the radius described in A3.'),
      excel('C4', 'Result'),

      shouldBe('A5', valueD(3.141592654)),
      shouldBe('A6', valueD(1.570796327)),
      shouldBe('A7', valueD(28.27433388)),

      exec(done)
    ]);
  });
  it('POWER', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=POWER(5,2)'),
      excel('A3', '=POWER(98.6,3.2)'),
      excel('A4', '=POWER(4,5/4)'),
      excel('B1', 'Description'),
      excel('B2', '5 squared.'),
      excel('B3', '98.6 raised to the power of 3.2.'),
      excel('B4', '4 raised to the power of 5/4.'),
      excel('C1', 'Result'),

      shouldBe('A2', valueI(25)),
      shouldBe('A3', valueD(2401077.222)),
      shouldBe('A4', valueD(5.656854249)),

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
  it('QUOTIENT', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=QUOTIENT(5, 2)'),
      excel('A3', '=QUOTIENT(4.5, 3.1)'),
      excel('A4', '=QUOTIENT(-10, 3)'),
      excel('B1', 'Description'),
      excel('B2', 'Integer portion of 5/2'),
      excel('B3', 'Integer portion of 4.5/3.1'),
      excel('B4', 'Integer portion of -10/3'),
      excel('C1', 'Result'),

      shouldBe('A2', valueI(2)),
      shouldBe('A3', valueI(1)),
      shouldBe('A4', valueI(-3)),

      exec(done)
    ]);
  });
  it('RADIANS', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=RADIANS(270)'),
      excel('B1', 'Description (Result)'),
      excel('B2', '270 degrees as radians (4.712389 or 3/2 radians)'),
      excel('C1', 'Result'),

      shouldBe('A2', valueD(4.712389)),

      exec(done)
    ]);
  });
  it('RAND', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=RAND()'),
      excel('A3', '=RAND()*100'),
      excel('B1', 'Description'),
      excel('B2', 'A random number greater than or equal to 0 and less than 1 (varies)'),
      excel('B3', 'A random number greater than or equal to 0 but less than 100 (varies)'),
      excel('B4', 'Note: When a worksheet is recalculated by entering a formula or data in a different cell, or by manually recalculating (press F9), a new random number is generated for any formula that uses the RAND function.'),
      excel('C1', 'Result'),

      shouldBe('A2', valueS('varies')),
      shouldBe('A3', valueS('varies')),

      exec(done)
    ]);
  });
  it('RANDBETWEEN', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=RANDBETWEEN(1,100)'),
      excel('A3', '=RANDBETWEEN(-1,1)'),
      excel('B1', 'Description'),
      excel('B2', 'Random number between 1 and 100 (varies)'),
      excel('B3', 'Random number between -1 and 1 (varies)'),
      excel('B4', 'Note: When a worksheet is recalculated by entering a formula or data in a different cell, or by manually recalculating (press F9), a new random number is generated for any formula that uses the RANDBETWEEN function.'),
      excel('C1', 'Result'),

      shouldBe('A2', valueS('varies')),
      shouldBe('A3', valueS('varies')),

      exec(done)
    ]);
  });
  it('ROMAN', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=ROMAN(499,0)'),
      excel('A3', '=ROMAN(499,1)'),
      excel('A4', '=ROMAN(499,2)'),
      excel('A5', '=ROMAN(499,3)'),
      excel('A6', '=ROMAN(499,4)'),
      excel('B1', 'Description (Result)'),
      excel('B2', 'Classic roman numeral style for 499 (CDXCIX)'),
      excel('B3', 'More concise version for 499 (LDVLIV)'),
      excel('B4', 'More concise version for 499 (XDIX)'),
      excel('B5', 'More concise version for 499 (VDIV)'),
      excel('B6', 'Simplified version for 499 (ID)'),
      excel('C1', 'Result'),

      shouldBe('A2', valueS('CDXCIX')),
      shouldBe('A3', valueS('LDVLIV')),
      shouldBe('A4', valueS('XDIX')),
      shouldBe('A5', valueS('VDIV')),
      shouldBe('A6', valueS('ID')),

      exec(done)
    ]);
  });
  it('ROUND', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=ROUND(2.15, 1)'),
      excel('A3', '=ROUND(2.149, 1)'),
      excel('A4', '=ROUND(-1.475, 2)'),
      excel('A5', '=ROUND(21.5, -1)'),
      excel('A6', '=ROUND(626.3,-3)'),
      excel('A7', '=ROUND(1.98,-1)'),
      excel('A8', '=ROUND(-50.55,-2)'),
      excel('B1', 'Description'),
      excel('B2', 'Rounds 2.15 to one decimal place'),
      excel('B3', 'Rounds 2.149 to one decimal place'),
      excel('B4', 'Rounds -1.475 to two decimal places'),
      excel('B5', 'Rounds 21.5 to one decimal place to the left of the decimal point'),
      excel('B6', 'Rounds 626.3 to the nearest multiple of 1000'),
      excel('B7', 'Rounds 1.98 to the nearest multiple of 10'),
      excel('B8', 'Rounds -50.55 to the nearest multiple of 100'),
      excel('C1', 'Result'),

      shouldBe('A2', valueD(2.2)),
      shouldBe('A3', valueD(2.1)),
      shouldBe('A4', valueD(-1.48)),
      shouldBe('A5', valueI(20)),
      shouldBe('A6', valueI(1000)),
      shouldBe('A7', valueI(0)),
      shouldBe('A8', valueI(-100)),

      exec(done)
    ]);
  });
  it('ROUNDDOWN', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=ROUNDDOWN(3.2, 0)'),
      excel('A3', '=ROUNDDOWN(76.9,0)'),
      excel('A4', '=ROUNDDOWN(3.14159, 3)'),
      excel('A5', '=ROUNDDOWN(-3.14159, 1)'),
      excel('A6', '=ROUNDDOWN(31415.92654, -2)'),
      excel('B1', 'Description'),
      excel('B2', 'Rounds 3.2 down to zero decimal places.'),
      excel('B3', 'Rounds 76.9 down to zero decimal places.'),
      excel('B4', 'Rounds 3.14159 down to three decimal places.'),
      excel('B5', 'Rounds -3.14159 down to one decimal place.'),
      excel('B6', 'Rounds 31415.92654 down to 2 decimal places to the left of the decimal point.'),
      excel('C1', 'Result'),

      shouldBe('A2', valueI(3)),
      shouldBe('A3', valueI(76)),
      shouldBe('A4', valueD(3.141)),
      shouldBe('A5', valueD(-3.1)),
      shouldBe('A6', valueI(31400)),

      exec(done)
    ]);
  });
  it('ROUNDUP', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=ROUNDUP(3.2,0)'),
      excel('A3', '=ROUNDUP(76.9,0)'),
      excel('A4', '=ROUNDUP(3.14159, 3)'),
      excel('A5', '=ROUNDUP(-3.14159, 1)'),
      excel('A6', '=ROUNDUP(31415.92654, -2)'),
      excel('B1', 'Description (Result)'),
      excel('B2', 'Rounds 3.2 up to zero decimal places.'),
      excel('B3', 'Rounds 76.9 up to zero decimal places.'),
      excel('B4', 'Rounds 3.14159 up to three decimal places.'),
      excel('B5', 'Rounds -3.14159 up to one decimal place.'),
      excel('B6', 'Rounds 31415.92654 up to 2 decimal places to the left of the decimal point.'),
      excel('C1', 'Result'),

      shouldBe('A2', valueI(4)),
      shouldBe('A3', valueI(77)),
      shouldBe('A4', valueD(3.142)),
      shouldBe('A5', valueD(-3.2)),
      shouldBe('A6', valueI(31500)),

      exec(done)
    ]);
  });
  it('SEC', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=SEC(45)'),
      excel('A4', '=SEC(30)'),
      excel('B1', 'Description'),
      excel('B2', 'Returns the secant of a 45 degree angle.'),
      excel('B4', 'Returns the secant of a 30 degree angle.'),
      excel('C1', 'Result'),

      shouldBe('A2', valueD(1.90359)),
      shouldBe('A4', valueD(6.48292)),

      exec(done)
    ]);
  });
  it('SECH', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=SECH(45)'),
      excel('A4', '=SECH(30)'),
      excel('B1', 'Description'),
      excel('B2', 'The hyperbolic secant of a 45 degree angle.'),
      excel('B4', 'The hyperbolic secant of a 30 degree angle.'),
      excel('C1', 'Result'),

      shouldBe('A2', valueD(5.73E-20)),
      shouldBe('A4', valueD(1.87E-13)),

      exec(done)
    ]);
  });
  it('SERIESSUM', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A2', 'Coefficients as numbers'),
      excel('A3', '0.785398163'),
      excel('A4', '1'),
      excel('A5', '-0.5'),
      excel('A6', '0.041666667'),
      excel('A7', '-0.001388889'),
      excel('A8', 'Formula'),
      excel('A9', '=SERIESSUM(A3,0,2,A4:A7)'),
      excel('B2', 'Coefficients as formulae'),
      excel('B3', '=PI()/4'),
      excel('B4', '1'),
      excel('B5', '=-1/FACT(2)'),
      excel('B6', '=1/FACT(4)'),
      excel('B7', '=-1/FACT(6)'),
      excel('B8', 'Description (Result)'),
      excel('B9', 'Approximation to the cosine of Pi/4 radians, or 45 degrees (0.707103)'),
      excel('C8', 'Result'),

      shouldBe('A9', valueD(0.707103)),

      exec(done)
    ]);
  });
  it('SIGN', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=SIGN(10)'),
      excel('A3', '=SIGN(4-4)'),
      excel('A4', '=SIGN(-0.00001)'),
      excel('B1', 'Description'),
      excel('B2', 'Sign of a positive number.'),
      excel('B3', 'Sign of the result of 4 minus 4 (zero).'),
      excel('B4', 'Sign of a negative number.'),
      excel('C1', 'Result'),

      shouldBe('A2', valueI(1)),
      shouldBe('A3', valueI(0)),
      shouldBe('A4', valueI(-1)),

      exec(done)
    ]);
  });
  it('SIN', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=SIN(PI())'),
      excel('A3', '=SIN(PI()/2)'),
      excel('A4', '=SIN(30*PI()/180)'),
      excel('A5', '=SIN(RADIANS(30))'),
      excel('B1', 'Description'),
      excel('B2', 'Sine of pi radians (0, approximately).'),
      excel('B3', 'Sine of pi/2 radians.'),
      excel('B4', 'Sine of 30 degrees.'),
      excel('B5', 'Sine of 30 degrees.'),
      excel('C1', 'Result'),

      shouldBe('A2', valueD(0.0)),
      shouldBe('A3', valueD(1.0)),
      shouldBe('A4', valueD(0.5)),
      shouldBe('A5', valueD(0.5)),

      exec(done)
    ]);
  });
  it('SINH', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=2.868*SINH(0.0342*1.03)'),
      excel('B1', 'Description'),
      excel('B2', 'Probability of obtaining a result of less than 1.03 seconds.'),
      excel('C1', 'R esult'),

      shouldBe('A2', valueD(0.1010491)),

      exec(done)
    ]);
  });
  it('SQRT', (done) => {
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
  it('SQRTPI', (done) => {
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
  it('SUBTOTAL', (done) => {
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
  it('SUMIF', (done) => {
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
      excel('C2', '$   2,300.00'),
      excel('C3', '$   5,500.00'),
      excel('C4', '$      800.00'),
      excel('C5', '$      400.00'),
      excel('C6', '$   4,200.00'),
      excel('C7', '$   1,200.00'),
      excel('C8', 'Result'),

      shouldBe('A10', valueS('$ 12,000.00')),
      shouldBe('A11', valueS('$   4,300.00')),
      shouldBe('A12', valueS('$      400.00')),
      shouldBe('A9', valueS('$   2,000.00')),

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
  it('TAN', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=TAN(0.785)'),
      excel('A3', '=TAN(45*PI()/180)'),
      excel('A4', '=TAN(RADIANS(45))'),
      excel('B1', 'Description (Result)'),
      excel('B2', 'Tangent of 0.785 radians (0.99920)'),
      excel('B3', 'Tangent of 45 degrees (1)'),
      excel('B4', 'Tangent of 45 degrees (1)'),
      excel('C1', 'Result'),

      shouldBe('A2', valueD(0.99920)),
      shouldBe('A3', valueI(1)),
      shouldBe('A4', valueI(1)),

      exec(done)
    ]);
  });
  it('TANH', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=TANH(-2)'),
      excel('A3', '=TANH(0)'),
      excel('A4', '=TANH(0.5)'),
      excel('B1', 'Description (Result)'),
      excel('B2', 'Hyperbolic tangent of -2 (-0.96403)'),
      excel('B3', 'Hyperbolic tangent of 0 (0)'),
      excel('B4', 'Hyperbolic tangent of 0.5 (0.462117)'),
      excel('C1', 'Result'),

      shouldBe('A2', valueD(-0.964028)),
      shouldBe('A3', valueI(0)),
      shouldBe('A4', valueD(0.462117)),

      exec(done)
    ]);
  });
  it('TRUNC', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=TRUNC(8.9)'),
      excel('A3', '=TRUNC(-8.9)'),
      excel('A4', '=TRUNC(0.45)'),
      excel('B1', 'Description'),
      excel('B2', 'Truncates 8.9 to return the integer part (8).'),
      excel('B3', 'Truncates a negative number to return the integer part (-8).'),
      excel('B4', 'Truncates a number between 0 and 1, returning the integer part (0).'),
      excel('C1', 'Result'),

      shouldBe('A2', valueI(8)),
      shouldBe('A3', valueI(-8)),
      shouldBe('A4', valueI(0)),

      exec(done)
    ]);
  });
  it('AVEDEV', (done) => {
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
  it('AVERAGE', (done) => {
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
  it('AVERAGEA', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A2', '10'),
      excel('A3', '7'),
      excel('A4', '9'),
      excel('A5', '2'),
      excel('A6', 'Not available'),
      excel('A7', 'Formula'),
      excel('A8', '=AVERAGEA(A2:A6)'),
      excel('A9', '=AVERAGEA(A2:A5,A7)'),
      excel('B7', 'Description'),
      excel('B8', 'Average of the numbers above, and the text "Not Available". The cell with the text "Not available" is used in the calculation.'),
      excel('B9', 'Average of the numbers above, and the empty cell.'),
      excel('C7', 'Result'),

      shouldBe('A8', valueD(5.6)),
      shouldBe('A9', valueD(5.6)),

      exec(done)
    ]);
  });
  it('AVERAGEIF', (done) => {
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

      shouldBe('A10', valueI(24500)),
      shouldBe('A7', valueI(14000)),
      shouldBe('A8', valueI(150000)),
      shouldBe('A9', valueS('#DIV/0!')),

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

      shouldBe('A8', valueD(16733.5)),
      shouldBe('A9', valueI(18589)),

      exec(done)
    ]);
  });
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

      shouldBe('A10', valueS('#DIV/0!')),
      shouldBe('A11', valueD(87.5)),
      shouldBe('A9', valueI(75)),

      exec(done)
    ]);
  });
  it('AVERAGEIFS', (done) => {
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
  it('BETADIST', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A2', '2'),
      excel('A3', '8'),
      excel('A4', '10'),
      excel('A5', '1'),
      excel('A6', '3'),
      excel('A7', 'Formula'),
      excel('A8', '=BETA.DIST(A2,A3,A4,TRUE,A5,A6)'),
      excel('A9', '=BETA.DIST(A2,A3,A4,FALSE,A5,A6)'),
      excel('B1', 'Description'),
      excel('B2', 'Value at which to evaluate the function'),
      excel('B3', 'Parameter of the distribution'),
      excel('B4', 'Parameter of the distribution'),
      excel('B5', 'Lower bound'),
      excel('B6', 'Upper bound'),
      excel('B7', 'Description'),
      excel('B8', 'Cumulative beta probability density function, for the above parameters'),
      excel('B9', 'Beta probability density function, for the above parameters'),
      excel('C7', 'Result'),

      shouldBe('A8', valueD(0.6854706)),
      shouldBe('A9', valueD(1.4837646)),

      exec(done)
    ]);
  });
  it('BETAINV', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A2', '0.685470581'),
      excel('A3', '8'),
      excel('A4', '10'),
      excel('A5', '1'),
      excel('A6', '3'),
      excel('A7', 'Formula'),
      excel('A8', '=BETA.INV(A2,A3,A4,A5,A6)'),
      excel('B1', 'Description'),
      excel('B2', 'Probability associated with the beta distribution'),
      excel('B3', 'Parameter of the distribution'),
      excel('B4', 'Parameter of the distribution'),
      excel('B5', 'Lower bound'),
      excel('B6', 'Upper bound'),
      excel('B7', 'Description'),
      excel('B8', 'Inverse of the cumulative beta probability density function for the parameters above.'),
      excel('C7', 'Result'),

      shouldBe('A8', valueI(2)),

      exec(done)
    ]);
  });
  it('BINOMDIST', (done) => {
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
  it('BINOMDISTRANGE', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=BINOM.DIST.RANGE(60,0.75,48)'),
      excel('A4', '=BINOM.DIST.RANGE(60,0.75,45,50)'),
      excel('B1', 'Description'),
      excel('B2', 'Returns the binomial distribution based on the probability of 48 successes in 60 trials and a 75% probability of success (0.084, or 8.4%).'),
      excel('B4', 'Returns the binomial distribution based on the probability of between 45 and 50 successes (inclusive) in 60 trials and a 75% probability of success (0.524, or 52.4%).'),
      excel('C1', 'Result'),

      shouldBe('A2', valueD(0.084)),
      shouldBe('A4', valueD(0.524)),

      exec(done)
    ]);
  });
  it('BINOMINV', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A2', '6'),
      excel('A3', '0.5'),
      excel('A4', '0.75'),
      excel('A5', 'Formula'),
      excel('A6', '=BINOM.INV(A2,A3,A4)'),
      excel('B1', 'Description'),
      excel('B2', 'Number of Bernoulli trials'),
      excel('B3', 'Probability of a success on each trial'),
      excel('B4', 'Criterion value'),
      excel('B5', 'Description'),
      excel('B6', 'Smallest value for which the cumulative binomial distribution is greater than or equal to a criterion value.'),
      excel('C5', 'Result'),

      shouldBe('A6', valueI(4)),

      exec(done)
    ]);
  });
  it('CHISQDIST', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=CHISQ.DIST(0.5,1,TRUE)'),
      excel('A3', '=CHISQ.DIST(2,3,FALSE)'),
      excel('B1', 'Description'),
      excel('B2', 'The chi-squared distribution for 0.5, returned as the cumulative distribution function, using 1 degree of freedom.'),
      excel('B3', 'The chi-squared distribution for 2, returned as the probability density function, using 3 degrees of freedom.'),
      excel('C1', 'Result'),

      shouldBe('A2', valueD(0.52049988)),
      shouldBe('A3', valueD(0.20755375)),

      exec(done)
    ]);
  });
  it('CHISQDISTRT', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A2', '18.307'),
      excel('A3', '10'),
      excel('A4', 'Formula'),
      excel('A5', '=CHISQ.DIST.RT(A2,A3)'),
      excel('B1', 'Description'),
      excel('B2', 'Value at which you want to evaluate the distribution'),
      excel('B3', 'Degrees of freedom'),
      excel('B4', 'Description'),
      excel('B5', 'One-tailed probability of the chi-squared distribution, for the arguments specified in A2 and A3.'),
      excel('C4', 'Result'),

      shouldBe('A5', valueD(0.0500006)),

      exec(done)
    ]);
  });
  it('CHISQINV', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=CHISQ.INV(0.93,1)'),
      excel('A3', '=CHISQ.INV(0.6,2)'),
      excel('B1', 'Description'),
      excel('B2', 'Inverse of the left-tailed probability of the chi-squared distribution for 0.93, using 1 degree of freedom.'),
      excel('B3', 'Inverse of the left-tailed probability of the chi-squared distribution for 0.6, using 2 degrees of freedom.'),
      excel('C1', 'Result'),

      shouldBe('A2', valueD(5.318520074)),
      shouldBe('A3', valueD(1.832581464)),

      exec(done)
    ]);
  });
  it('CHISQINVRT', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A2', '0.050001'),
      excel('A3', '10'),
      excel('A4', 'Formula'),
      excel('A5', '=CHISQ.INV.RT(A2,A3)'),
      excel('B1', 'Description'),
      excel('B2', 'Probability associated with the chi-squared distribution'),
      excel('B3', 'Degrees of freedom'),
      excel('B4', 'Description'),
      excel('B5', 'Inverse of the one-tailed probability of the chi-squared distribution'),
      excel('C4', 'Result'),

      shouldBe('A5', valueD(18.306973)),

      exec(done)
    ]);
  });
  it('CHISQTEST', (done) => {
    _do([
      excel('A1', 'Men (Actual)'),
      excel('A10', '=CHISQ.TEST(A2:B4,A6:B8)'),
      excel('A2', '58'),
      excel('A3', '11'),
      excel('A4', '10'),
      excel('A5', 'Men (Expected)'),
      excel('A6', '45.35'),
      excel('A7', '17.56'),
      excel('A8', '16.09'),
      excel('A9', 'Formula'),
      excel('B1', 'Women (Actual)'),
      excel('B10', 'The 2 statistic for the data above is 16.16957 with 2 degrees of freedom'),
      excel('B2', '35'),
      excel('B3', '25'),
      excel('B4', '23'),
      excel('B5', 'Women (Expected)'),
      excel('B6', '47.65'),
      excel('B7', '18.44'),
      excel('B8', '16.91'),
      excel('B9', 'Description'),
      excel('C1', 'Description'),
      excel('C2', 'Agree'),
      excel('C3', 'Neutral'),
      excel('C4', 'Disagree'),
      excel('C5', 'Description'),
      excel('C6', 'Agree'),
      excel('C7', 'Neutral'),
      excel('C8', 'Disagree'),
      excel('C9', 'Result'),

      shouldBe('A10', valueD(0.0003082)),

      exec(done)
    ]);
  });
  it('CONFIDENCENORM', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A2', '0.05'),
      excel('A3', '2.5'),
      excel('A4', '50'),
      excel('A5', 'Formula'),
      excel('A6', '=CONFIDENCE.NORM(A2,A3,A4)'),
      excel('B1', 'Description'),
      excel('B2', 'Significance level'),
      excel('B3', 'Standard deviation of the population'),
      excel('B4', 'Sample size'),
      excel('B5', 'Description'),
      excel('B6', 'Confidence interval for a population mean. In other words, the confidence interval for the underlying population mean for travel to work equals 30  0.692952 minutes, or 29.3 to 30.7 minutes.'),
      excel('C5', 'Result'),

      shouldBe('A6', valueD(0.692952)),

      exec(done)
    ]);
  });
  it('CONFIDENCET', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=CONFIDENCE.T(0.05,1,50)'),
      excel('B1', 'Description'),
      excel('B2', 'Confidence interval for the mean of a population based on a sample size of 50, with a 5% significance level and a standard deviation of 1. This is based on a Student\'s t-distribution.'),
      excel('C1', 'Result'),

      shouldBe('A2', valueD(0.284196855)),

      exec(done)
    ]);
  });
  it('CORREL', (done) => {
    _do([
      excel('A1', 'Data1'),
      excel('A2', '3'),
      excel('A3', '2'),
      excel('A4', '4'),
      excel('A5', '5'),
      excel('A6', '6'),
      excel('A7', 'Formula'),
      excel('A8', '=CORREL(A2:A6,B2:B6)'),
      excel('B1', 'Data2'),
      excel('B2', '9'),
      excel('B3', '7'),
      excel('B4', '12'),
      excel('B5', '15'),
      excel('B6', '17'),
      excel('B7', 'Description'),
      excel('B8', 'Correlation coefficient of the two data sets in columns A and B.'),
      excel('C7', 'Result'),

      shouldBe('A8', valueD(0.997054486)),

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
  it('COUNTA', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A2', '39790'),
      excel('A4', '19'),
      excel('A5', '22.24'),
      excel('A6', 'TRUE'),
      excel('A7', '#DIV/0!'),
      excel('A8', 'Formula'),
      excel('A9', '=COUNTA(A2:A7)'),
      excel('B8', 'Description'),
      excel('B9', 'Counts the number of nonblank cells in cells A2 through A7.'),
      excel('C8', 'Result'),

      shouldBe('A9', valueI(5)),

      exec(done)
    ]);
  });
  it('COUNTBLANK', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A3', '6'),
      excel('A5', '4'),
      excel('A6', 'Formula'),
      excel('A7', '=COUNTBLANK(A2:B5)'),
      excel('B1', 'Data'),
      excel('B4', '27'),
      excel('B5', '34'),
      excel('B6', 'Description'),
      excel('B7', 'Counts empty cells in the range above. The formula returns empty text.'),
      excel('B8', 'Note You need to turn iterative calculation off to run this formula. Click the File tab, click Options, click Formulas, and under Calculation options, clear the Enable iterative calculation check box.'),
      excel('C6', 'Result'),

      shouldBe('A7', valueI(4)),

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
  it('COUNTIFS', (done) => {
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
  it('COVARIANCEP', (done) => {
    _do([
      excel('A1', 'Data1'),
      excel('A2', '3'),
      excel('A3', '2'),
      excel('A4', '4'),
      excel('A5', '5'),
      excel('A6', '6'),
      excel('A7', 'Formula'),
      excel('A8', '=COVARIANCE.P(A2:A6, B2:B6)'),
      excel('B1', 'Data2'),
      excel('B2', '9'),
      excel('B3', '7'),
      excel('B4', '12'),
      excel('B5', '15'),
      excel('B6', '17'),
      excel('B7', 'Description'),
      excel('B8', 'Covariance, the average of the products of deviations for each data point pair above'),
      excel('C7', 'Result'),

      shouldBe('A8', valueD(5.2)),

      exec(done)
    ]);
  });
  it('COVARIANCES', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=COVARIANCE.S({2,4,8},{5,11,12})'),
      excel('A3', '2'),
      excel('A4', '4'),
      excel('A5', '8'),
      excel('A6', 'Formula'),
      excel('A7', '=COVARIANCE.S(A3:A5,B3:B5)'),
      excel('B1', 'Description'),
      excel('B2', 'Sample covariance for the data points entered as an array in the function.'),
      excel('B3', '5'),
      excel('B4', '11'),
      excel('B5', '12'),
      excel('B6', 'Description'),
      excel('B7', 'Sample covariance for the identical data points, but entered as cell ranges in the function.'),
      excel('C1', 'Result'),
      excel('C6', 'Result'),

      shouldBe('A2', valueD(9.666666667)),
      shouldBe('A7', valueD(9.666666667)),

      exec(done)
    ]);
  });
  it('DEVSQ', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A10', '=DEVSQ(A2:A8)'),
      excel('A2', '4'),
      excel('A3', '5'),
      excel('A4', '8'),
      excel('A5', '7'),
      excel('A6', '11'),
      excel('A7', '4'),
      excel('A8', '3'),
      excel('A9', 'Formula'),
      excel('B10', 'Sum of squares of deviations of data above from their sample mean.'),
      excel('B9', 'Description'),
      excel('C9', 'Result'),

      shouldBe('A10', valueI(48)),

      exec(done)
    ]);
  });
  it('EXPONDIST', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A2', '0.2'),
      excel('A3', '10'),
      excel('A4', 'Formula'),
      excel('A5', '=EXPON.DIST(A2,A3,TRUE)'),
      excel('A6', '=EXPON.DIST(0.2,10,FALSE)'),
      excel('B1', 'Description'),
      excel('B2', 'Value of the function'),
      excel('B3', 'Parameter value'),
      excel('B4', 'Description'),
      excel('B5', 'Cumulative exponential distribution function'),
      excel('B6', 'Probability exponential distribution function'),
      excel('C4', 'Result'),

      shouldBe('A5', valueD(0.86466472)),
      shouldBe('A6', valueD(1.35335283)),

      exec(done)
    ]);
  });
  it('FDIST', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A2', '15.2069'),
      excel('A3', '6'),
      excel('A4', '4'),
      excel('A5', 'Formula'),
      excel('A6', '=F.DIST(A2,A3,A4,TRUE)'),
      excel('A7', '=F.DIST(A2,A3,A4,FALSE)'),
      excel('B1', 'Description'),
      excel('B2', 'Value at which to evaluate the function'),
      excel('B3', 'Numerator degrees of freedom'),
      excel('B4', 'Denominator degrees of freedom'),
      excel('B5', 'Description'),
      excel('B6', 'F probability using the cumulative distribution function (TRUE cumulative argument).'),
      excel('B7', 'F probability using the probability density function (FALSE cumulative argument).'),
      excel('C5', 'Result'),

      shouldBe('A6', valueD(0.0012238)),
      shouldBe('A7', valueD(0.99)),

      exec(done)
    ]);
  });
  it('FDISTRT', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A2', '15.2068649'),
      excel('A3', '6'),
      excel('A4', '4'),
      excel('A5', 'Formula'),
      excel('A6', '=F.DIST.RT(A2,A3,A4)'),
      excel('B1', 'Description'),
      excel('B2', 'Value at which to evaluate the function'),
      excel('B3', 'Numerator degrees of freedom'),
      excel('B4', 'Denominator degrees of freedom'),
      excel('B5', 'Description'),
      excel('B6', 'F probability distribution for the terms above.'),
      excel('C5', 'Result'),

      shouldBe('A6', valueD(0.01)),

      exec(done)
    ]);
  });
  it('FINV', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A2', '0.01'),
      excel('A3', '6'),
      excel('A4', '4'),
      excel('A5', 'Formula'),
      excel('A6', '=F.INV(A2,A3,A4)'),
      excel('B1', 'Description'),
      excel('B2', 'Probability associated with the F cumulative distribution'),
      excel('B3', 'Numerator degrees of freedom'),
      excel('B4', 'Denominator degrees of freedom'),
      excel('B5', 'Description'),
      excel('B6', 'Inverse of the F probability distribution for the terms above'),
      excel('C5', 'Result'),

      shouldBe('A6', valueD(0.10930991)),

      exec(done)
    ]);
  });
  it('FINVRT', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A2', '0.01'),
      excel('A3', '6'),
      excel('A4', '4'),
      excel('A5', 'Formula'),
      excel('A6', '=F.INV.RT(A2,A3,A4)'),
      excel('B1', 'Description'),
      excel('B2', 'Probability associated with the F cumulative distribution'),
      excel('B3', 'Numerator degrees of freedom'),
      excel('B4', 'Denominator degrees of freedom'),
      excel('B5', 'Description'),
      excel('B6', 'Inverse of the F probability distribution for the terms above'),
      excel('C5', 'Result'),

      shouldBe('A6', valueD(15.20686)),

      exec(done)
    ]);
  });
  it('FTEST', (done) => {
    _do([
      excel('A1', 'Data1'),
      excel('A2', '6'),
      excel('A3', '7'),
      excel('A4', '9'),
      excel('A5', '15'),
      excel('A6', '21'),
      excel('A7', 'Formula'),
      excel('A8', '=F.TEST(A2:A6,B2:B6)'),
      excel('B1', 'Data2'),
      excel('B2', '20'),
      excel('B3', '28'),
      excel('B4', '31'),
      excel('B5', '38'),
      excel('B6', '40'),
      excel('B7', 'Description'),
      excel('B8', 'F-test for the data sets in A2:A6 and B2:B6.'),
      excel('C7', 'Result'),

      shouldBe('A8', valueD(0.64831785)),

      exec(done)
    ]);
  });
  it('FISHER', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=FISHER(0.75)'),
      excel('B1', 'Description'),
      excel('B2', 'Fisher transformation at 0.75'),
      excel('C1', 'Result'),

      shouldBe('A2', valueD(0.9729551)),

      exec(done)
    ]);
  });
  it('FISHERINV', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=FISHERINV(0.972955)'),
      excel('B1', 'Description'),
      excel('B2', 'Inverse of the Fisher transformation at 0.972955'),
      excel('C1', 'Result'),

      shouldBe('A2', valueD(0.75)),

      exec(done)
    ]);
  });
  it('FORECAST', (done) => {
    _do([
      excel('A1', 'Known Y'),
      excel('A2', '6'),
      excel('A3', '7'),
      excel('A4', '9'),
      excel('A5', '15'),
      excel('A6', '21'),
      excel('A7', 'Formula'),
      excel('A8', '=FORECAST(30,A2:A6,B2:B6)'),
      excel('B1', 'Known X'),
      excel('B2', '20'),
      excel('B3', '28'),
      excel('B4', '31'),
      excel('B5', '38'),
      excel('B6', '40'),
      excel('B7', 'Description'),
      excel('B8', 'Predicts a value for y given an x value of 30'),
      excel('C7', 'Result'),

      shouldBe('A8', valueD(10.607253)),

      exec(done)
    ]);
  });
  it('FREQUENCY', (done) => {
    _do([
      excel('A1', 'Scores'),
      excel('A10', '97'),
      excel('A11', 'Formula'),
      excel('A12', '=FREQUENCY(A2:A10, B2:B4)'),
      excel('A16', 'Note: The formula in cell C12 is an array formula. For this formula to return values in cells C12,C13,C14 and C15, you must open this workbook in the Excel desktop program (not the web browser). In Excel, select C12,C13,C14 and C15, press F2, and then press CTRL+Shift+Enter. Otherwise, only a value in cell C12 will be returned.'),
      excel('A2', '79'),
      excel('A3', '85'),
      excel('A4', '78'),
      excel('A5', '85'),
      excel('A6', '50'),
      excel('A7', '81'),
      excel('A8', '95'),
      excel('A9', '88'),
      excel('B1', 'Bins'),
      excel('B11', 'Description'),
      excel('B12', 'Number of scores less than or equal to 70'),
      excel('B13', 'Number of scores in the bin 71-79'),
      excel('B14', 'Number of scores in the bin 80-89'),
      excel('B15', 'Number of scores greater than or equal to 90'),
      excel('B2', '70'),
      excel('B3', '79'),
      excel('B4', '89'),
      excel('C11', 'Result'),
      excel('C13', '2'),
      excel('C14', '4'),
      excel('C15', '2'),

      shouldBe('A12', valueI(1)),

      exec(done)
    ]);
  });
  it('GAMMA', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=GAMMA(2.5)'),
      excel('A3', '=GAMMA(-3.75)'),
      excel('A4', '=GAMMA(0)'),
      excel('A5', '=GAMMA(-2)'),
      excel('B1', 'Description'),
      excel('B2', 'Returns the gamma function value of 2.5 (1.329).'),
      excel('B3', 'Returns the gamma function value of -3.75 (0.268).'),
      excel('B4', 'Returns the #NUM! error value, because 0 is not a valid argument.'),
      excel('B5', 'Returns the #NUM! error value, because a negative integer is not a valid argument.'),
      excel('C1', 'Result'),

      shouldBe('A2', valueD(1.329)),
      shouldBe('A3', valueD(0.268)),
      shouldBe('A4', valueS('#NUM!')),
      shouldBe('A5', valueS('#NUM!')),

      exec(done)
    ]);
  });
  it('GAMMADIST', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A2', '10.00001131'),
      excel('A3', '9'),
      excel('A4', '2'),
      excel('A5', 'Formula'),
      excel('A6', '=GAMMA.DIST(A2,A3,A4,FALSE)'),
      excel('A7', '=GAMMA.DIST(A2,A3,A4,TRUE)'),
      excel('B1', 'Description'),
      excel('B2', 'Value at which you want to evaluate the distribution'),
      excel('B3', 'Alpha parameter to the distribution'),
      excel('B4', 'Beta parameter to the distribution'),
      excel('B5', 'Description'),
      excel('B6', 'Probability density using the x, alpha, and beta values in A2, A3, A4, with FALSE cumulative argument.'),
      excel('B7', 'Cumulative distributuion  using the x, alpha, and beta values in A2, A3, A4, with TRUE cumulative argument.'),
      excel('C5', 'Result'),

      shouldBe('A6', valueD(0.032639)),
      shouldBe('A7', valueD(0.068094)),

      exec(done)
    ]);
  });
  it('GAMMAINV', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A2', '0.068094'),
      excel('A3', '9'),
      excel('A4', '2'),
      excel('A5', 'Formula'),
      excel('A6', '=GAMMA.INV(A2,A3,A4)'),
      excel('B1', 'Description'),
      excel('B2', 'Probability associated with the gamma distribution'),
      excel('B3', 'Alpha parameter to the distribution'),
      excel('B4', 'Beta parameter to the distribution'),
      excel('B5', 'Description'),
      excel('B6', 'Inverse of the gamma cumulative distribution for the probability, alpha, and beta arguments in A2, A3, and A4.'),
      excel('C5', 'Result'),

      shouldBe('A6', valueD(10.0000112)),

      exec(done)
    ]);
  });
  it('GAMMALN', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=GAMMALN(4)'),
      excel('B1', 'Description'),
      excel('B2', 'Natural logarithm of the gamma function at 4.'),
      excel('C1', 'Result'),

      shouldBe('A2', valueD(1.7917595)),

      exec(done)
    ]);
  });
  it('GAMMALNPRECISE', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=GAMMALN.PRECISE(4)'),
      excel('B1', 'Description'),
      excel('B2', 'Natural logarithm of the gamma function at 4'),
      excel('C1', 'Result'),

      shouldBe('A2', valueD(1.7917595)),

      exec(done)
    ]);
  });
  it('GAUSS', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '\'=GAUSS(2)'),
      excel('B1', 'Description'),
      excel('B2', 'Probability that a member of a standard normal population will fall between the mean and 2 standard deviations from the mean (result is 0.47725).'),
      excel('C1', 'Result'),

      shouldBe('A2', valueS('=GAUSS(2)')),

      exec(done)
    ]);
  });
  it('GEOMEAN', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A10', '=GEOMEAN(A2:A8)'),
      excel('A2', '4'),
      excel('A3', '5'),
      excel('A4', '8'),
      excel('A5', '7'),
      excel('A6', '11'),
      excel('A7', '4'),
      excel('A8', '3'),
      excel('A9', 'Formula'),
      excel('B10', 'Geometric mean of the data set contained in A2:A8.'),
      excel('B9', 'Description'),
      excel('C9', 'Result'),

      shouldBe('A10', valueD(5.476987)),

      exec(done)
    ]);
  });
  it('HARMEAN', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A10', '=HARMEAN(A2:A8)'),
      excel('A2', '4'),
      excel('A3', '5'),
      excel('A4', '8'),
      excel('A5', '7'),
      excel('A6', '11'),
      excel('A7', '4'),
      excel('A8', '3'),
      excel('A9', 'Formula'),
      excel('B10', 'Harmonic mean of the data set in A2:A8.'),
      excel('B9', 'Description'),
      excel('C9', 'Result'),

      shouldBe('A10', valueD(5.028376)),

      exec(done)
    ]);
  });
  it('HYPGEOMDIST', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A2', '1'),
      excel('A3', '4'),
      excel('A4', '8'),
      excel('A5', '20'),
      excel('A6', 'Formula'),
      excel('A7', '=HYPGEOM.DIST(A2,A3,A4,A5,TRUE)'),
      excel('A9', '=HYPGEOM.DIST(A2,A3,A4,A5,FALSE)'),
      excel('B1', 'Description'),
      excel('B2', 'Number of successes in the sample'),
      excel('B3', 'Sample size'),
      excel('B4', 'Number of successes in the population'),
      excel('B5', 'Population size'),
      excel('B6', 'Description (Result)'),
      excel('B7', 'Cumulative hypergeometric distribution function, for sample and population in cells A2 through A5.'),
      excel('B9', 'Probability hypergeometric distribution function, for sample and in cells A2 through A5.'),
      excel('C1', 'Result'),

      shouldBe('A7', valueD(0.4654)),
      shouldBe('A9', valueD(0.3633)),

      exec(done)
    ]);
  });
  it('INTERCEPT', (done) => {
    _do([
      excel('A1', 'Known y'),
      excel('A2', '2'),
      excel('A3', '3'),
      excel('A4', '9'),
      excel('A5', '1'),
      excel('A6', '8'),
      excel('A7', 'Formula'),
      excel('A8', '=INTERCEPT(A2:A6, B2:B6)'),
      excel('B1', 'Known x'),
      excel('B2', '6'),
      excel('B3', '5'),
      excel('B4', '11'),
      excel('B5', '7'),
      excel('B6', '5'),
      excel('B7', 'Description'),
      excel('B8', 'Point at which a line will intersect the y-axis by using the x-values and y-values above'),
      excel('C7', 'Result'),

      shouldBe('A8', valueD(0.0483871)),

      exec(done)
    ]);
  });
  it('KURT', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A10', '4'),
      excel('A11', '7'),
      excel('A12', 'Formula'),
      excel('A13', '=KURT(A2:A11)'),
      excel('A2', '3'),
      excel('A3', '4'),
      excel('A4', '5'),
      excel('A5', '2'),
      excel('A6', '3'),
      excel('A7', '4'),
      excel('A8', '5'),
      excel('A9', '6'),
      excel('B12', 'Description'),
      excel('B13', 'Kurtosis of the data set above'),
      excel('C12', 'Result'),

      shouldBe('A13', valueD(-0.151799637)),

      exec(done)
    ]);
  });
  it('LARGE', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A2', '3'),
      excel('A3', '5'),
      excel('A4', '3'),
      excel('A5', '5'),
      excel('A6', '4'),
      excel('A7', 'Formula'),
      excel('A8', '=LARGE(A2:B6,3)'),
      excel('A9', '=LARGE(A2:B6,7)'),
      excel('B1', 'Data'),
      excel('B2', '4'),
      excel('B3', '2'),
      excel('B4', '4'),
      excel('B5', '6'),
      excel('B6', '7'),
      excel('B7', 'Description'),
      excel('B8', '3rd largest number in the numbers above'),
      excel('B9', '7th largest number in the numbers above'),
      excel('C7', 'Result'),

      shouldBe('A8', valueI(5)),
      shouldBe('A9', valueI(4)),

      exec(done)
    ]);
  });
  it('LINEST', (done) => {
    _do([
      excel('A1', 'Month'),
      excel('A2', '1'),
      excel('A3', '2'),
      excel('A4', '3'),
      excel('A5', '4'),
      excel('A6', '5'),
      excel('A7', '6'),
      excel('A8', 'Formula'),
      excel('A9', '=SUM(LINEST(B1:B6, A1:A6)*{9,1})'),
      excel('B1', 'Sales'),
      excel('B10', 'Calculates the estimate of the sales in the ninth month, based on sales in months 2 through 6.'),
      excel('B2', '$3,100'),
      excel('B3', '$4,500'),
      excel('B4', '$4,400'),
      excel('B5', '$5,400'),
      excel('B6', '$7,500'),
      excel('B7', '$8,100'),
      excel('B8', 'Result'),

      shouldBe('A9', valueS('$11,000')),

      exec(done)
    ]);
  });
  it('LOGEST', (done) => {
    _do([
      excel('A1', 'Month'),
      excel('A2', '11'),
      excel('A3', '12'),
      excel('A4', '13'),
      excel('A5', '14'),
      excel('A6', '15'),
      excel('A7', '16'),
      excel('A8', 'Formula'),
      excel('A9', '=LOGEST(B2:B7,A2:A7, TRUE, FALSE)'),
      excel('B1', 'Units'),
      excel('B2', '33100'),
      excel('B3', '47300'),
      excel('B4', '69000'),
      excel('B5', '102000'),
      excel('B6', '150000'),
      excel('B7', '220000'),
      excel('B8', 'Description'),
      excel('B9', 'Note   The formula in the example must be entered as an array formula in the Excel program. After copying the example to a blank worksheet, select the range C9:D9 starting with the formula cell. Press F2, and then press CTRL+SHIFT+ENTER. If the formula is not entered as an array formula, the single result is 1.4633.'),
      excel('C8', 'Result'),
      excel('D9', '495.3048'),

      shouldBe('A9', valueD(1.4633)),

      exec(done)
    ]);
  });
  it('LOGEST', (done) => {
    _do([
      excel('A1', 'Month'),
      excel('A2', '11'),
      excel('A3', '12'),
      excel('A4', '13'),
      excel('A5', '14'),
      excel('A6', '15'),
      excel('A7', '16'),
      excel('A8', 'Formula'),
      excel('A9', '=LOGEST(B2:B7,A2:A7,TRUE,TRUE)'),
      excel('B1', 'Units'),
      excel('B2', '33,100'),
      excel('B3', '47,300'),
      excel('B4', '69,000'),
      excel('B5', '102,000'),
      excel('B6', '150,000'),
      excel('B7', '220,000'),
      excel('B8', 'Result'),

      shouldBe('A9', valueD(1.4633)),

      exec(done)
    ]);
  });
  it('LOGNORMDIST', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A2', '4'),
      excel('A3', '3.5'),
      excel('A4', '1.2'),
      excel('A5', 'Formula'),
      excel('A6', '=LOGNORM.DIST(A2,A3,A4,TRUE)'),
      excel('A7', '=LOGNORM.DIST(A2,A3,A4,FALSE)'),
      excel('B1', 'Description'),
      excel('B2', 'Value at which to evaluate the function (x)'),
      excel('B3', 'Mean of ln(x)'),
      excel('B4', 'Standard deviation of ln(x)'),
      excel('B5', 'Description'),
      excel('B6', 'Cumulative lognormal distribution at 4, using the arguments in A2:A4.'),
      excel('B7', 'Probability lognormal distribution at 4, using the same arguments.'),
      excel('C5', 'Result'),

      shouldBe('A6', valueD(0.0390836)),
      shouldBe('A7', valueD(0.0176176)),

      exec(done)
    ]);
  });
  it('LOGNORMINV', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A2', '0.039084'),
      excel('A3', '3.5'),
      excel('A4', '1.2'),
      excel('A5', 'Formula'),
      excel('A6', '=LOGNORM.INV(A2, A3, A4)'),
      excel('B1', 'Description'),
      excel('B2', 'Probability associated with the lognormal distribution'),
      excel('B3', 'Mean of ln(x)'),
      excel('B4', 'Standard deviation of ln(x)'),
      excel('B5', 'Description'),
      excel('B6', 'Inverse of the lognormal cumulative distribution function for the terms above'),
      excel('C5', 'Result'),

      shouldBe('A6', valueD(4.0000252)),

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
  it('MAXA', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A2', '0'),
      excel('A3', '0.2'),
      excel('A4', '0.5'),
      excel('A5', '0.4'),
      excel('A6', 'TRUE'),
      excel('A7', 'Formula'),
      excel('A8', '=MAXA(A2:A6)'),
      excel('B7', 'Description'),
      excel('B8', 'The largest number in the range A2:A6. Because a TRUE value evaluates to 1, it is the largest.'),
      excel('C7', 'Result'),

      shouldBe('A8', valueI(1)),

      exec(done)
    ]);
  });
  it('MEDIAN', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A10', '=MEDIAN(A2:A7)'),
      excel('A2', '1'),
      excel('A3', '2'),
      excel('A4', '3'),
      excel('A5', '4'),
      excel('A6', '5'),
      excel('A7', '6'),
      excel('A8', 'Formula'),
      excel('A9', '=MEDIAN(A2:A6)'),
      excel('B10', 'Median of  the 6 numbers in the range A2:A7. Because there are six numbers, the median is the midway point between the third and fourth numbers.'),
      excel('B8', 'Description'),
      excel('B9', 'Median of the 5 numbers in the range A2:A6. Because there are 5 values, the third is the median.'),
      excel('C8', 'Result'),

      shouldBe('A10', valueD(3.5)),
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
  it('MINA', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A2', 'FALSE'),
      excel('A3', '0.2'),
      excel('A4', '0.5'),
      excel('A5', '0.4'),
      excel('A6', '0.8'),
      excel('A7', 'Formula'),
      excel('A8', '=MINA(A2:A6)'),
      excel('B7', 'Description'),
      excel('B8', 'Smallest of the numbers in the range A2:A6. Because a value of FALSE evaluates to 0, it is the smallest.'),
      excel('C7', 'Result'),

      shouldBe('A8', valueI(0)),

      exec(done)
    ]);
  });
  it('MODEMULT', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A10', '3'),
      excel('A11', '5'),
      excel('A12', '6'),
      excel('A13', '1'),
      excel('A14', 'Formula'),
      excel('A15', '=MODE.MULT(A2:A13)'),
      excel('A2', '1'),
      excel('A3', '2'),
      excel('A4', '3'),
      excel('A5', '4'),
      excel('A6', '3'),
      excel('A7', '2'),
      excel('A8', '1'),
      excel('A9', '2'),
      excel('B14', 'Description'),
      excel('B15', 'The formula =MODE.MULT(A2:A13)must be entered as an array formula. When entered as an array formula, MODE.MULT returns 1, 2, and 3 as the modes because they each appear 3 times. If the formula is not entered as an array formula, the single result is 1. This would be the same result as using the MODE.SNGL function. When I created the array formula, I included several extra cells to make sure that all modes were returned. I built the array formula in the range C15:C22. Where there are no additonal modes, there are #N/A error values.'),
      excel('C14', 'Result'),
      excel('C16', '2'),
      excel('C17', '3'),
      excel('C18', '#N/A'),
      excel('C19', '#N/A'),
      excel('C20', '#N/A'),
      excel('C21', '#N/A'),
      excel('C22', '#N/A'),

      shouldBe('A15', valueI(1)),

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
  it('NEGBINOMDIST', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A2', '10'),
      excel('A3', '5'),
      excel('A4', '0.25'),
      excel('A5', 'Formula'),
      excel('A6', '=NEGBINOM.DIST(A2,A3,A4,TRUE)'),
      excel('A7', '=NEGBINOM.DIST(A2,A3,A4,FALSE)'),
      excel('B1', 'Description'),
      excel('B2', 'Number of failures'),
      excel('B3', 'Threshold number of successes'),
      excel('B4', 'Probability of a success'),
      excel('B5', 'Description'),
      excel('B6', 'Cumulative negative binomial distribution for the terms above'),
      excel('B7', 'Probability negative binomial distribution for the terms above'),
      excel('C5', 'Result'),

      shouldBe('A6', valueD(0.3135141)),
      shouldBe('A7', valueD(0.0550487)),

      exec(done)
    ]);
  });
  it('NORMDIST', (done) => {
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
  it('NORMINV', (done) => {
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
  it('NORMSDIST', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=NORM.S.DIST(1.333333,TRUE)'),
      excel('A3', '=NORM.S.DIST(1.333333,FALSE)'),
      excel('B1', 'Description'),
      excel('B2', 'Normal cumulative distribution function at 1.333333'),
      excel('B3', 'Normal probability distribution function at 1.333333'),
      excel('C1', 'Result'),

      shouldBe('A2', valueD(0.908788726)),
      shouldBe('A3', valueD(0.164010148)),

      exec(done)
    ]);
  });
  it('NORMSINV', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=NORM.S.INV(0.908789)'),
      excel('B1', 'Description'),
      excel('B2', 'Inverse of the standard normal cumulative distribution, with a probability of 0.908789'),
      excel('C1', 'Live Result'),

      shouldBe('A2', valueD(1.3333347)),

      exec(done)
    ]);
  });
  it('PEARSON', (done) => {
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
  it('PERCENTILEEXC', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A10', '9'),
      excel('A11', 'Formula'),
      excel('A12', '=PERCENTILE.EXC(A2:A10, 0.25)'),
      excel('A13', '=PERCENTILE.EXC(A2:A10, 0)'),
      excel('A14', '=PERCENTILE.EXC(A2:A10, 0.01)'),
      excel('A15', '=PERCENTILE.EXC(A2:A10, 2)'),
      excel('A2', '1'),
      excel('A3', '2'),
      excel('A4', '3'),
      excel('A5', '6'),
      excel('A6', '6'),
      excel('A7', '6'),
      excel('A8', '7'),
      excel('A9', '8'),
      excel('B11', 'Description'),
      excel('B12', 'Interpolates when the value for the specified percentile lies between two values in the array.'),
      excel('B13', 'Because it can\'t interpolate for the specified percentile, Excel returns the #NUM! error message.'),
      excel('B14', 'Because it can\'t interpolate for the specified percentile, Excel returns the #NUM! error message.'),
      excel('B15', 'Because the percentile specified is greater than 1, Excel returns the #NUM! error message.'),
      excel('C11', 'Result'),

      shouldBe('A12', valueD(2.5)),
      shouldBe('A13', valueS('#NUM!')),
      shouldBe('A14', valueS('#NUM!')),
      shouldBe('A15', valueS('#NUM!')),

      exec(done)
    ]);
  });
  it('PERCENTILEINC', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A2', '1'),
      excel('A3', '3'),
      excel('A4', '2'),
      excel('A5', '4'),
      excel('A6', 'Formula'),
      excel('A7', '=PERCENTILE.INC(A2:A5,0.3)'),
      excel('B6', 'Description'),
      excel('B7', '30th percentile of the list in the range A2:A5.'),
      excel('C6', 'Result'),

      shouldBe('A7', valueD(1.9)),

      exec(done)
    ]);
  });
  it('PERCENTRANKEXC', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A10', '9'),
      excel('A11', 'Formula'),
      excel('A12', '=PERCENTRANK.EXC(A2:A10, 7)'),
      excel('A13', '=PERCENTRANK.EXC(A2:A10,5.43)'),
      excel('A14', '=PERCENTRANK.EXC(A2:A10,5.43,1)'),
      excel('A2', '1'),
      excel('A3', '2'),
      excel('A4', '3'),
      excel('A5', '6'),
      excel('A6', '6'),
      excel('A7', '6'),
      excel('A8', '7'),
      excel('A9', '8'),
      excel('B11', 'Description'),
      excel('B12', 'Returns the rank of the value 7 from the array contained in A2:A10.'),
      excel('B13', 'Returns the rank of the value 5.43 in the same array.'),
      excel('B14', 'Returns the rank of the value 5.43 in the same array, displaying only 1 significant digit in the result (the default is 3).'),
      excel('C11', 'Result'),

      shouldBe('A12', valueD(0.7)),
      shouldBe('A13', valueD(0.381)),
      shouldBe('A14', valueD(0.3)),

      exec(done)
    ]);
  });
  it('PERCENTRANKINC', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A10', '1'),
      excel('A11', '1'),
      excel('A12', 'Formula'),
      excel('A13', '=PERCENTRANK.INC(A2:A11,2)'),
      excel('A14', '=PERCENTRANK.INC(A2:A11,4)'),
      excel('A15', '=PERCENTRANK.INC(A2:A11,8)'),
      excel('A16', '=PERCENTRANK.INC(A2:A11,5)'),
      excel('A2', '13'),
      excel('A3', '12'),
      excel('A4', '11'),
      excel('A5', '8'),
      excel('A6', '4'),
      excel('A7', '3'),
      excel('A8', '2'),
      excel('A9', '1'),
      excel('B12', 'Description'),
      excel('B13', 'Percent rank of 2 in the range A2:A11 (0.333, because 3 values in the set are smaller than 2, and 6 are larger than 2; 3/(3+6)=0.333).'),
      excel('B14', 'Percent rank of 4 in the range A2:A11.'),
      excel('B15', 'Percent rank of 8 in the range A2:A11.'),
      excel('B16', 'Percent rank of 5 in the range A2:A11 (0.583, one-quarter of the way between the PERCENTRANK.INC of 4 and the PERCENTRANK.INC of 8).'),
      excel('C12', 'Result'),

      shouldBe('A13', valueD(0.333)),
      shouldBe('A14', valueD(0.555)),
      shouldBe('A15', valueD(0.666)),
      shouldBe('A16', valueD(0.583)),

      exec(done)
    ]);
  });
  it('PERMUT', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A2', '100'),
      excel('A3', '3'),
      excel('A4', 'Formula'),
      excel('A5', '=PERMUT(A2,A3)'),
      excel('A6', '=PERMUT(3,2)'),
      excel('B1', 'Description'),
      excel('B2', 'Number of objects'),
      excel('B3', 'Number of objects in each permutation'),
      excel('B4', 'Description'),
      excel('B5', 'Permutations possible for the arguments specified in A2:A3.'),
      excel('B6', 'Permutations possible for a group of 3 objects where 2 are chosen.'),
      excel('C4', 'Result'),

      shouldBe('A5', valueI(970200)),
      shouldBe('A6', valueI(6)),

      exec(done)
    ]);
  });
  it('PERMUTATIONA', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A13', '=PERMUTATIONA(2,2)'),
      excel('A2', '=PERMUTATIONA(3,2)'),
      excel('B1', 'Description'),
      excel('B10', '6,5'),
      excel('B11', '6,6'),
      excel('B13', 'Suppose there are 2 objects in the group, [3,5]. Using PERMUTATIONA with both of the objects, there are 4 ways the numbers can be arranged with repetition:'),
      excel('B14', '3,3'),
      excel('B15', '3,5'),
      excel('B16', '5,3'),
      excel('B17', '5,5'),
      excel('B2', 'Suppose there are 3 objects in the group, [4,5,6]. Using PERMUTATIONA with 2 of the 3 objects, there are 9 ways the numbers can be arranged with repetition:'),
      excel('B3', '4,4'),
      excel('B4', '4,5'),
      excel('B5', '4,6'),
      excel('B6', '5,4'),
      excel('B7', '5,5'),
      excel('B8', '5,6'),
      excel('B9', '6,4'),
      excel('C1', 'Result'),

      shouldBe('A13', valueI(4)),
      shouldBe('A2', valueI(9)),

      exec(done)
    ]);
  });
  it('PHI', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=PHI(0.75)'),
      excel('B1', 'Description'),
      excel('B2', 'The value of the density function for a standard normal distribution.'),
      excel('C1', 'Result'),

      shouldBe('A2', valueD(0.301137432)),

      exec(done)
    ]);
  });
  it('POISSONDIST', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A2', '2'),
      excel('A3', '5'),
      excel('A4', 'Formula'),
      excel('A5', '=POISSON.DIST(A2,A3,TRUE)'),
      excel('A6', '=POISSON.DIST(A2,A3,FALSE)'),
      excel('B1', 'Description'),
      excel('B2', 'Number of events'),
      excel('B3', 'Expected mean'),
      excel('B4', 'Description'),
      excel('B5', 'Cumulative Poisson probability with the arguments specified in A2 and A3.'),
      excel('B6', 'Poisson probability mass function with the arguments specified in A2 and A3.'),
      excel('C4', 'Result'),

      shouldBe('A5', valueD(0.124652)),
      shouldBe('A6', valueD(0.084224)),

      exec(done)
    ]);
  });
  it('PROB', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A2', 'Value of x'),
      excel('A3', '0'),
      excel('A4', '1'),
      excel('A5', '2'),
      excel('A6', '3'),
      excel('A7', 'Formula'),
      excel('A8', '=PROB(A3:A6,B3:B6,2)'),
      excel('A9', '=PROB(A3:A6,B3:B6,1,3)'),
      excel('B2', 'Probability'),
      excel('B3', '0.2'),
      excel('B4', '0.3'),
      excel('B5', '0.1'),
      excel('B6', '0.4'),
      excel('B7', 'Description'),
      excel('B8', 'Probability that x is 2.'),
      excel('B9', 'Probability that x is between 1 and 3.'),
      excel('C7', 'Result'),

      shouldBe('A8', valueD(0.1)),
      shouldBe('A9', valueD(0.8)),

      exec(done)
    ]);
  });
  it('QUARTILEEXC', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A10', '43'),
      excel('A11', '47'),
      excel('A12', '49'),
      excel('A13', 'Formula'),
      excel('A14', '=QUARTILE.EXC(A2:A12,1)'),
      excel('A15', '=QUARTILE.EXC(A2:A12,3)'),
      excel('A2', '6'),
      excel('A3', '7'),
      excel('A4', '15'),
      excel('A5', '36'),
      excel('A6', '39'),
      excel('A7', '40'),
      excel('A8', '41'),
      excel('A9', '42'),
      excel('B13', 'Description'),
      excel('B14', 'Locates the position of the first quartile (15).'),
      excel('B15', 'Locates the position of the third quartile (43).'),
      excel('C13', 'Result'),

      shouldBe('A14', valueI(15)),
      shouldBe('A15', valueI(43)),

      exec(done)
    ]);
  });
  it('QUARTILEINC', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A10', 'Formula'),
      excel('A11', '=QUARTILE.INC(A2:A9,1)'),
      excel('A2', '1'),
      excel('A3', '2'),
      excel('A4', '4'),
      excel('A5', '7'),
      excel('A6', '8'),
      excel('A7', '9'),
      excel('A8', '10'),
      excel('A9', '12'),
      excel('B10', 'Description (Result)'),
      excel('B11', 'First quartile (25th percentile) of the data above (3.5)'),
      excel('C10', 'Result'),

      shouldBe('A11', valueD(3.5)),

      exec(done)
    ]);
  });
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
  it('RSQ', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A10', 'Formula'),
      excel('A11', '=RSQ(A3:A9, B3:B9)'),
      excel('A2', 'Known y'),
      excel('A3', '2'),
      excel('A4', '3'),
      excel('A5', '9'),
      excel('A6', '1'),
      excel('A7', '8'),
      excel('A8', '7'),
      excel('A9', '5'),
      excel('B10', 'Description'),
      excel('B11', 'Square of the Pearson product moment correlation coefficient through data points in A3:A9 and B3:B9.'),
      excel('B2', 'Known x'),
      excel('B3', '6'),
      excel('B4', '5'),
      excel('B5', '11'),
      excel('B6', '7'),
      excel('B7', '5'),
      excel('B8', '4'),
      excel('B9', '4'),
      excel('C10', 'Result'),

      shouldBe('A11', valueD(0.05795)),

      exec(done)
    ]);
  });
  it('SKEW', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A10', '4'),
      excel('A11', '7'),
      excel('A12', 'Formula'),
      excel('A13', '=SKEW(A2:A11)'),
      excel('A2', '3'),
      excel('A3', '4'),
      excel('A4', '5'),
      excel('A5', '2'),
      excel('A6', '3'),
      excel('A7', '4'),
      excel('A8', '5'),
      excel('A9', '6'),
      excel('B12', 'Description'),
      excel('B13', 'Skewness of a distribution of the data set in A2:A11.'),
      excel('C12', 'Result'),

      shouldBe('A13', valueD(0.359543)),

      exec(done)
    ]);
  });
  it('SKEWP', (done) => {
    _do([
      excel('A1', 'Population Data Set'),
      excel('A10', '4'),
      excel('A11', '7'),
      excel('A12', 'Formula'),
      excel('A13', '=SKEW.P(A2:A11)'),
      excel('A2', '3'),
      excel('A3', '4'),
      excel('A4', '5'),
      excel('A5', '2'),
      excel('A6', '3'),
      excel('A7', '4'),
      excel('A8', '5'),
      excel('A9', '6'),
      excel('B12', 'Description'),
      excel('B13', 'Skewness of a distribution based on the population of the data set in A2:A11 (0.303193).'),
      excel('C12', 'Result'),

      shouldBe('A13', valueD(0.303193)),

      exec(done)
    ]);
  });
  it('SLOPE', (done) => {
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
  it('SMALL', (done) => {
    _do([
      excel('A1', 'Data 1'),
      excel('A10', '7'),
      excel('A11', 'Formula'),
      excel('A12', '=SMALL(A2:A10,4)'),
      excel('A13', '=SMALL(B2:B10,2)'),
      excel('A2', '3'),
      excel('A3', '4'),
      excel('A4', '5'),
      excel('A5', '2'),
      excel('A6', '3'),
      excel('A7', '4'),
      excel('A8', '6'),
      excel('A9', '4'),
      excel('B1', 'Data 2'),
      excel('B10', '23'),
      excel('B11', 'Description (Result)'),
      excel('B12', '4th smallest number in first column (4)'),
      excel('B13', '2nd smallest number in the second column (3)'),
      excel('B2', '1'),
      excel('B3', '4'),
      excel('B4', '8'),
      excel('B5', '3'),
      excel('B6', '7'),
      excel('B7', '12'),
      excel('B8', '54'),
      excel('B9', '8'),
      excel('C11', 'Result'),

      shouldBe('A12', valueI(4)),
      shouldBe('A13', valueI(3)),

      exec(done)
    ]);
  });
  it('STANDARDIZE', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A2', '42'),
      excel('A3', '40'),
      excel('A4', '1.5'),
      excel('A5', 'Formula'),
      excel('A6', '=STANDARDIZE(A2,A3,A4)'),
      excel('B1', 'Description'),
      excel('B2', 'Value to normalize.'),
      excel('B3', 'Arithmetic mean of the distribution.'),
      excel('B4', 'Standard deviation of the distribution.'),
      excel('B5', 'Description'),
      excel('B6', 'Normalized value of 42, using 40 as the arithmetic mean and 1.5 as the standard deviation.'),
      excel('C5', 'Result'),

      shouldBe('A6', valueD(1.33333333)),

      exec(done)
    ]);
  });
  it('STDEVP', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A10', '1350'),
      excel('A11', '1303'),
      excel('A12', '1299'),
      excel('A13', 'Formula'),
      excel('A14', '=STDEV.P(A3:A12)'),
      excel('A2', 'Strength'),
      excel('A3', '1345'),
      excel('A4', '1301'),
      excel('A5', '1368'),
      excel('A6', '1322'),
      excel('A7', '1310'),
      excel('A8', '1370'),
      excel('A9', '1318'),
      excel('B13', 'Description'),
      excel('B14', 'Standard deviation of breaking strength, assuming only 10 tools are produced.'),
      excel('C13', 'Result'),

      shouldBe('A14', valueD(26.05455814)),

      exec(done)
    ]);
  });
  it('STDEVS', (done) => {
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
  it('STDEVA', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A10', '1350'),
      excel('A11', '1303'),
      excel('A12', '1299'),
      excel('A13', 'Formula'),
      excel('A14', '=STDEVA(A3:A12)'),
      excel('A2', 'Strength'),
      excel('A3', '1345'),
      excel('A4', '1301'),
      excel('A5', '1368'),
      excel('A6', '1322'),
      excel('A7', '1310'),
      excel('A8', '1370'),
      excel('A9', '1318'),
      excel('B13', 'Description (Result)'),
      excel('B14', 'Standard deviation of breaking strength for all the tools (27.46391572)'),
      excel('C13', 'Result'),

      shouldBe('A14', valueD(27.46391572)),

      exec(done)
    ]);
  });
  it('STDEVPA', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A10', '1350'),
      excel('A11', '1303'),
      excel('A12', '1299'),
      excel('A13', 'Formula'),
      excel('A14', '=STDEVPA(A3:A12)'),
      excel('A2', 'Strength'),
      excel('A3', '1345'),
      excel('A4', '1301'),
      excel('A5', '1368'),
      excel('A6', '1322'),
      excel('A7', '1310'),
      excel('A8', '1370'),
      excel('A9', '1318'),
      excel('B13', 'Description (Result)'),
      excel('B14', 'Standard deviation of breaking strength, assuming only 10 tools are produced (26.05455814)'),
      excel('C13', 'Result'),

      shouldBe('A14', valueD(26.05456)),

      exec(done)
    ]);
  });
  it('STEYX', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A10', 'Formula'),
      excel('A11', '=STEYX(A3:A9,B3:B9)'),
      excel('A2', 'Known y'),
      excel('A3', '2'),
      excel('A4', '3'),
      excel('A5', '9'),
      excel('A6', '1'),
      excel('A7', '8'),
      excel('A8', '7'),
      excel('A9', '5'),
      excel('B10', 'Description (Result)'),
      excel('B11', 'Standard error of the predicted y-value for each x in the regression (3.305719)'),
      excel('B2', 'Known x'),
      excel('B3', '6'),
      excel('B4', '5'),
      excel('B5', '11'),
      excel('B6', '7'),
      excel('B7', '5'),
      excel('B8', '4'),
      excel('B9', '4'),
      excel('C10', 'Result'),

      shouldBe('A11', valueD(3.305719)),

      exec(done)
    ]);
  });
  it('TDIST', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=T.DIST(60,1,TRUE)'),
      excel('A3', '=T.DIST(8,3,FALSE)'),
      excel('B1', 'Description'),
      excel('B2', 'Student\'s left-tailed t-distribution for 60, returned as the cumulative distribution function, using 1 degree of freedom.'),
      excel('B3', 'Student\'s left-tailed t-distribution for 8, returned as the probability density function, using 3 degrees of freedom.'),
      excel('C1', 'Result'),

      shouldBe('A2', valueD(0.99469533)),
      shouldBe('A3', valueD(0.00073691)),

      exec(done)
    ]);
  });
  it('TDIST2T', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A2', '1.959999998'),
      excel('A3', '60'),
      excel('A4', 'Formula'),
      excel('A5', '=T.DIST.2T(A2,A3)'),
      excel('B1', 'Description'),
      excel('B2', 'Value at which to evaluate the distribution'),
      excel('B3', 'Degrees of freedom'),
      excel('B4', 'Description (Result)'),
      excel('B5', 'Two-tailed distribution (0.054645, or 5.46 percent)'),
      excel('C4', 'Result'),

      shouldBe('A5', valueS('5.46%')),

      exec(done)
    ]);
  });
  it('TDISTRT', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A2', '1.959999998'),
      excel('A3', '60'),
      excel('A4', 'Formula'),
      excel('A5', '=T.DIST.RT(A2,A3)'),
      excel('B1', 'Description'),
      excel('B2', 'Value at which to evaluate the distribution'),
      excel('B3', 'Degrees of freedom'),
      excel('B4', 'Description (Result)'),
      excel('B5', 'Two-tailed distribution (0.027322, or 2.73 percent)'),
      excel('C4', 'Result'),

      shouldBe('A5', valueD(0.027322)),

      exec(done)
    ]);
  });
  it('TINV', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=T.INV(0.75,2)'),
      excel('B1', 'Description'),
      excel('B2', 'The left-tailed inverse of the Student\'s t-distribution with a probability of 75% and 2 degrees of freedom.'),
      excel('C1', 'Result'),

      shouldBe('A2', valueD(0.8164966)),

      exec(done)
    ]);
  });
  it('TINV2T', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A2', '0.546449'),
      excel('A3', '60'),
      excel('A4', 'Formula'),
      excel('A5', '=T.INV.2T(A2,A3)'),
      excel('B1', 'Description'),
      excel('B2', 'Probability associated with the two-tailed Student\'s t-distribution'),
      excel('B3', 'Degrees of freedom'),
      excel('B4', 'Description (Result)'),
      excel('B5', 'T-value of the Student\'s t-distribution for the terms above (0.606533076)'),
      excel('C4', 'Result'),

      shouldBe('A5', valueD(0.606533)),

      exec(done)
    ]);
  });
  it('TTEST', (done) => {
    _do([
      excel('A1', 'Data 1'),
      excel('A10', '5'),
      excel('A11', 'Formula'),
      excel('A12', '=T.TEST(A2:A10,B2:B10,2,1)'),
      excel('A2', '3'),
      excel('A3', '4'),
      excel('A4', '5'),
      excel('A5', '8'),
      excel('A6', '9'),
      excel('A7', '1'),
      excel('A8', '2'),
      excel('A9', '4'),
      excel('B1', 'Data 2'),
      excel('B10', '1'),
      excel('B11', 'Description'),
      excel('B12', 'Probability associated with a Student\'s paired t-Test, with a two-tailed distribution.'),
      excel('B2', '6'),
      excel('B3', '19'),
      excel('B4', '3'),
      excel('B5', '2'),
      excel('B6', '14'),
      excel('B7', '4'),
      excel('B8', '5'),
      excel('B9', '17'),
      excel('C11', 'Result'),

      shouldBe('A12', valueD(0.196016)),

      exec(done)
    ]);
  });
  it('TREND', (done) => {
    _do([
      excel('A1', 'Month'),
      excel('A10', '9'),
      excel('A11', '10'),
      excel('A12', '11'),
      excel('A13', '12'),
      excel('A14', 'Month'),
      excel('A15', '13'),
      excel('A16', '14'),
      excel('A17', '15'),
      excel('A18', '16'),
      excel('A19', '17'),
      excel('A2', '1'),
      excel('A21', 'In the Live Result column, the ranges D2:D13 and D15:D19 each contain one formula that is used for all cells in the range. Enter the formula =TREND(A2:A13,B2:B13) in D2:D13 with all its cells selected and then press CTRL+SHIFT+Enter to enter it as an array formula.Do the same with the range D15:D19, using the formula =TREND(B2:B13,A2:A13,A15:A19). You can copy these formulas from cells C2 and A15.'),
      excel('A3', '2'),
      excel('A4', '3'),
      excel('A5', '4'),
      excel('A6', '5'),
      excel('A7', '6'),
      excel('A8', '7'),
      excel('A9', '8'),
      excel('B1', 'Cost'),
      excel('B10', '$141,890'),
      excel('B11', '$143,230'),
      excel('B12', '$144,000'),
      excel('B13', '$145,290'),
      excel('B14', 'Formula (Predicted Cost)'),
      excel('B15', '=TREND(B2:B13,A2:A13,A15:A19)'),
      excel('B2', '$133,890'),
      excel('B3', '$135,000'),
      excel('B4', '$135,790'),
      excel('B5', '$137,300'),
      excel('B6', '$138,130'),
      excel('B7', '$139,100'),
      excel('B8', '$139,900'),
      excel('B9', '$141,120'),
      excel('C1', 'Formula (Corresponding Cost)'),
      excel('C2', '=TREND(A2:A13,B2:B13)'),
      excel('D1', 'Live Result'),
      excel('D10', '$142,099'),
      excel('D11', '$143,117'),
      excel('D12', '$144,135'),
      excel('D13', '$145,153'),
      excel('D14', 'Live Result'),
      excel('D16', '$147,190'),
      excel('D17', '$148,208'),
      excel('D18', '$149,226'),
      excel('D19', '$150,244'),
      excel('D3', '$134,972'),
      excel('D4', '$135,990'),
      excel('D5', '$137,008'),
      excel('D6', '$138,026'),
      excel('D7', '$139,044'),
      excel('D8', '$140,062'),
      excel('D9', '$141,081'),

      shouldBe('A15', valueS('$146,172')),
      shouldBe('A2', valueS('$133,953')),

      exec(done)
    ]);
  });
  it('TRIMMEAN', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A10', '1'),
      excel('A11', '2'),
      excel('A12', '3'),
      excel('A13', 'Formula'),
      excel('A14', '=TRIMMEAN(A2:A12,0.2)'),
      excel('A2', '4'),
      excel('A3', '5'),
      excel('A4', '6'),
      excel('A5', '7'),
      excel('A6', '2'),
      excel('A7', '3'),
      excel('A8', '4'),
      excel('A9', '5'),
      excel('B13', 'Description'),
      excel('B14', 'Mean of the interior of the data set contained in A2:A12, with 20 percent excluded from calculation.'),
      excel('C13', 'Result'),

      shouldBe('A14', valueD(3.778)),

      exec(done)
    ]);
  });
  it('VARP', (done) => {
    _do([
      excel('A1', 'Strength'),
      excel('A10', '1,303'),
      excel('A11', '1,299'),
      excel('A12', 'Formula'),
      excel('A13', '=VAR.P(A2:A11)'),
      excel('A14', '=VAR.S(A2:A11)'),
      excel('A2', '1,345'),
      excel('A3', '1,301'),
      excel('A4', '1,368'),
      excel('A5', '1,322'),
      excel('A6', '1,310'),
      excel('A7', '1,370'),
      excel('A8', '1,318'),
      excel('A9', '1,350'),
      excel('B12', 'Description'),
      excel('B13', 'Variance of breaking strengths for all the tools, assuming that only 10 tools are produced (the entire population is used).'),
      excel('B14', 'The variance, using the VAR.S function, which assumes only a sample of the population is tested. The result is different from VAR.P.'),
      excel('C12', 'Result'),

      shouldBe('A13', valueD(678.84)),
      shouldBe('A14', valueD(754.27)),

      exec(done)
    ]);
  });
  it('VARS', (done) => {
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
  it('VARA', (done) => {
    _do([
      excel('A1', 'Strength'),
      excel('A10', '1303'),
      excel('A11', '1299'),
      excel('A12', 'Formula'),
      excel('A13', '=VARA(A2:A11)'),
      excel('A2', '1345'),
      excel('A3', '1301'),
      excel('A4', '1368'),
      excel('A5', '1322'),
      excel('A6', '1310'),
      excel('A7', '1370'),
      excel('A8', '1318'),
      excel('A9', '1350'),
      excel('B12', 'Description'),
      excel('B13', 'Estimates the variance for the breaking strength of the tools being tested. VARA assumes a population sample.'),
      excel('C12', 'Result'),

      shouldBe('A13', valueD(754.26667)),

      exec(done)
    ]);
  });
  it('VARPA', (done) => {
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
  it('WEIBULLDIST', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A2', '105'),
      excel('A3', '20'),
      excel('A4', '100'),
      excel('A5', 'Formula'),
      excel('A6', '=WEIBULL.DIST(A2,A3,A4,TRUE)'),
      excel('A7', '=WEIBULL.DIST(A2,A3,A4,FALSE)'),
      excel('B1', 'Description'),
      excel('B2', 'Value at which to evaluate the function'),
      excel('B3', 'Alpha parameter to the distribution'),
      excel('B4', 'Beta parameter to the distribution'),
      excel('B5', 'Description (Result)'),
      excel('B6', 'Weibull cumulative distribution function for the terms above (0.929581)'),
      excel('B7', 'Weibull probability density function for the terms above (0.035589)'),
      excel('C5', 'Result'),

      shouldBe('A6', valueD(0.929581)),
      shouldBe('A7', valueD(0.035589)),

      exec(done)
    ]);
  });
  it('ZTEST', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A10', '1'),
      excel('A11', '9'),
      excel('A12', 'Formula'),
      excel('A13', '=Z.TEST(A2:A11,4)'),
      excel('A14', '=2 * MIN(Z.TEST(A2:A11,4), 1 - Z.TEST(A2:A11,4))'),
      excel('A15', '=Z.TEST(A2:A11,6)'),
      excel('A16', '=2 * MIN(Z.TEST(A2:A11,6), 1 - Z.TEST(A2:A11,6))'),
      excel('A2', '3'),
      excel('A3', '6'),
      excel('A4', '7'),
      excel('A5', '8'),
      excel('A6', '6'),
      excel('A7', '5'),
      excel('A8', '4'),
      excel('A9', '2'),
      excel('B12', 'Description (Result)'),
      excel('B13', 'One-tailed probability-value of a z-test for the data set above, at the hypothesized population mean of 4 (0.090574)'),
      excel('B14', 'Two-tailed probability-value of a z-test for the data set above, at the hypothesized population mean of 4 (0.181148)'),
      excel('B15', 'One-tailed probability-value of a z-test for the data set above, at the hypothesized population mean of 6 (0.863043)'),
      excel('B16', 'Two-tailed probability-value of a z-test for the data set above, at the hypothesized population mean of 6 (0.273913)'),
      excel('C12', 'Result'),

      shouldBe('A13', valueD(0.090574)),
      shouldBe('A14', valueD(0.181148)),
      shouldBe('A15', valueD(0.863043)),
      shouldBe('A16', valueD(0.273913)),

      exec(done)
    ]);
  });
  it('BAHTTEXT', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A2', '1234'),
      excel('A3', 'Formula'),
      excel('A4', '=BAHTTEXT(A2)'),
      excel('B3', 'Description'),
      excel('B4', 'Displays the number in text. (One thousand two hundred thirty four Baht in Thai text)'),
      excel('C3', 'Result'),

      shouldBe('A4', valueS('')),

      exec(done)
    ]);
  });
  it('CHAR', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=CHAR(65)'),
      excel('A3', '=CHAR(33)'),
      excel('B1', 'Description'),
      excel('B2', 'Displays the character represented by 65 in the computer\'s character set.'),
      excel('B3', 'Displays the character represented by 33 in the computer\'s character set.'),
      excel('C1', 'Result'),

      shouldBe('A2', valueS('A')),
      shouldBe('A3', valueS('!')),

      exec(done)
    ]);
  });
  it('CLEAN', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A2', '=CHAR(9)&"Monthly report"&CHAR(10)'),
      excel('A3', 'Formula'),
      excel('A4', '=CLEAN(A2)'),
      excel('B3', 'Description'),
      excel('B4', 'Removes the nonprintable characters CHAR(9) and CHAR(10) from the text string in cell A2.'),
      excel('C3', 'Result'),

      shouldBe('A4', valueS('Monthly report')),

      exec(done)
    ]);
  });
  it('CODE', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=CODE("A")'),
      excel('A3', '=CODE("!")'),
      excel('B1', 'Description'),
      excel('B2', 'Displays the numeric code for A'),
      excel('B3', 'Displays the numeric code for !'),
      excel('C1', 'Result'),

      shouldBe('A2', valueI(65)),
      shouldBe('A3', valueI(33)),

      exec(done)
    ]);
  });
  it('DOLLAR', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A10', '=DOLLAR(A4, 4)'),
      excel('A11', '=DOLLAR(A5)'),
      excel('A2', '1234.567'),
      excel('A3', '-1234.567'),
      excel('A4', '-0.123'),
      excel('A5', '99.888'),
      excel('A6', 'Formula'),
      excel('A7', '=DOLLAR(A2, 2)'),
      excel('A8', '=DOLLAR(A2, -2)'),
      excel('A9', '=DOLLAR(A3, -2)'),
      excel('B10', 'Displays the third number in a currency format, 4 digits to the right of the decimal point.'),
      excel('B11', 'Displays the fourth number in a currency format, 2 digit to the left of the decimal point .'),
      excel('B6', 'Description'),
      excel('B7', 'Displays the first number in a currency format, 2 digits to the right of the decimal point.'),
      excel('B8', 'Displays the first number in a currency format, 2 digits to the left of the decimal point.'),
      excel('B9', 'Displays the second number in a currency format, 2 digits to the left of the decimal point.'),
      excel('C6', 'Result'),

      shouldBe('A10', valueS('($0.1230)')),
      shouldBe('A11', valueS('$99.89')),
      shouldBe('A7', valueS('$1,234.57')),
      shouldBe('A8', valueS('$1,200')),
      shouldBe('A9', valueS('($1,200)')),

      exec(done)
    ]);
  });
  it('EXACT', (done) => {
    _do([
      excel('A1', 'First string'),
      excel('A2', 'word'),
      excel('A3', 'Word'),
      excel('A4', 'w ord'),
      excel('A5', 'Formula'),
      excel('A6', '=EXACT(A2,B2)'),
      excel('A7', '=EXACT(A3,B3)'),
      excel('A8', '=EXACT(A4,B4)'),
      excel('B1', 'Second string'),
      excel('B2', 'word'),
      excel('B3', 'word'),
      excel('B4', 'word'),
      excel('B5', 'Description'),
      excel('B6', 'Checks whether the strings in the first row match'),
      excel('B7', 'Checks whether the strings in the second row match ("W" is uppercase in A3)'),
      excel('B8', 'Checks whether the strings in the third row match  (A4 contains a space between "w" and "ord)'),
      excel('C5', 'Result'),

      shouldBe('A6', valueB(true)),
      shouldBe('A7', valueB(false)),
      shouldBe('A8', valueB(false)),

      exec(done)
    ]);
  });
  it('FIXED', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A2', '1234.567'),
      excel('A3', '-1234.567'),
      excel('A4', '44.332'),
      excel('A5', 'Formula'),
      excel('A6', '=FIXED(A2, 1)'),
      excel('A7', '=FIXED(A2, -1)'),
      excel('A8', '=FIXED(A3, -1, TRUE)'),
      excel('A9', '=FIXED(A4)'),
      excel('B5', 'Description'),
      excel('B6', 'Rounds the number in A2 one digit to the right of the decimal point.'),
      excel('B7', 'Rounds the number in A2 one digit to the left of the decimal point.'),
      excel('B8', 'Rounds the number in A3 one digit to the left of the decimal point, without commas (the TRUE argument).'),
      excel('B9', 'Rounds the number in A4 two digits to the left of the decimal point.'),
      excel('C5', 'Result'),

      shouldBe('A6', valueS('1,234.6')),
      shouldBe('A7', valueS('1,230')),
      shouldBe('A8', valueI(-1230)),
      shouldBe('A9', valueD(44.33)),

      exec(done)
    ]);
  });
  it('LOWER', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A2', 'E. E. Cummings'),
      excel('A3', 'Apt. 2B'),
      excel('A4', 'Formula'),
      excel('A5', '=LOWER(A2)'),
      excel('A6', '=LOWER(A3)'),
      excel('B4', 'Description'),
      excel('B5', 'Lower case of first string (e. e. cummings)'),
      excel('B6', 'Lower case of last string (apt. 2b)'),
      excel('C4', 'Result'),

      shouldBe('A5', valueS('e. e. cummings')),
      shouldBe('A6', valueS('apt. 2b')),

      exec(done)
    ]);
  });
  it('NUMBERVALUE', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=NUMBERVALUE("2.500,27",",",".")'),
      excel('A4', '=NUMBERVALUE("3.5%")'),
      excel('B1', 'Description'),
      excel('B2', 'Returns 2,500.27. The decimal separator of the text argument in the example is specified in the second argument as a comma, and the group separator is specified in the third argument as a period.'),
      excel('B4', 'Returns 0.035. Because no optional arguments are specified, the decimal and group separators of the current locale are used. The % symbol is not shown, although the percentage is calculated.'),
      excel('C1', 'Result'),

      shouldBe('A2', valueD(2500.27)),
      shouldBe('A4', valueD(0.035)),

      exec(done)
    ]);
  });
  it('PROPER', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A2', 'this is a TITLE'),
      excel('A3', '2-way street'),
      excel('A4', '76BudGet'),
      excel('A5', 'Formula'),
      excel('A6', '=PROPER(A2)'),
      excel('A7', '=PROPER(A3)'),
      excel('A8', '=PROPER(A4)'),
      excel('B5', 'Description'),
      excel('B6', 'Proper case of thestring in A2.'),
      excel('B7', 'Proper case of the string in A3.'),
      excel('B8', 'Proper case of the string in A4.'),
      excel('C5', 'Result'),

      shouldBe('A6', valueS('This Is A Title')),
      shouldBe('A7', valueS('2-Way Street')),
      shouldBe('A8', valueS('76Budget')),

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
  it('SUBSTITUTE', (done) => {
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
      shouldBe('A7', valueS('Quarter 2, 2008')),
      shouldBe('A8', valueS('Quarter 1, 2012')),

      exec(done)
    ]);
  });
  it('T', (done) => {
    _do([
      excel('A1', 'Data'),
      excel('A2', 'Rainfall'),
      excel('A3', '19'),
      excel('A4', 'TRUE'),
      excel('A5', 'Formula'),
      excel('A6', '=T(A2)'),
      excel('A7', '=T(A3)'),
      excel('A8', '=T(A4)'),
      excel('B5', 'Description (Result)'),
      excel('B6', 'Because the first value is text, the text is returned (Rainfall)'),
      excel('B7', 'Because the second value is a number, empty text is returned ()'),
      excel('B8', 'Because the third value is a logical value, empty text is returned ()'),
      excel('C5', 'Result'),

      shouldBe('A6', valueS('Rainfall')),

      exec(done)
    ]);
  });
  it('TEXT', (done) => {
    _do([
      excel('A1', 'Salesperson'),
      excel('A10', '=TEXT(C2, "$#,##0.00")'),
      excel('A2', 'Burke'),
      excel('A3', 'Dykstra'),
      excel('A4', 'Formula'),
      excel('A5', '=A2&" sold "&TEXT(B2, "$0.00")&" worth of units."'),
      excel('A6', '=A3&" had "&TEXT(B3, "0%")&" of the total sales."'),
      excel('A7', '="Date: " & TEXT(C2, "yyyy-mm-dd")'),
      excel('A8', '="Date-time: " & TEXT(C2, "m/d/yyyy h:mm AM/PM")'),
      excel('A9', '=TEXT(C2, "0.00E+00")'),
      excel('B1', 'Sales'),
      excel('B10', 'Displays the value in C2 in a currency format, with a thousands separator.'),
      excel('B2', '$2,800'),
      excel('B3', '40%'),
      excel('B4', 'Description'),
      excel('B5', 'Combines cell A2, the text string " sold," cell B2 (formatted as currency), and the text string " worth of units." into a phrase.'),
      excel('B6', 'Combines cell A3, the string " sold," cell B3 (formatted as a percentage), and the text string " of the total sales." into a phrase.'),
      excel('B7', 'Displays the value in C2 in a 4-digit year, 2-digit month, 2-digit day format.'),
      excel('B8', 'Displays the value in C2 in a short date, 12-hour time format.'),
      excel('B9', 'Displays the value in C2 in scientific (exponential) format.'),
      excel('C1', 'Data'),
      excel('C2', '39300.63'),
      excel('C4', 'Result'),

      shouldBe('A10', valueS('$39,300.63')),
      shouldBe('A5', valueS('Burke sold $2800.00 worth of units.')),
      shouldBe('A6', valueS('Dykstra had 40% of the total sales.')),
      shouldBe('A7', valueS('Date: 2007-08-06')),
      shouldBe('A8', valueS('Date-time: 8/6/2007 3:07 PM')),
      shouldBe('A9', valueD(3.93E+04)),

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
  it('UNICHAR', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=UNICHAR(66)'),
      excel('A3', '=UNICHAR(32)'),
      excel('A4', '=UNICHAR(0)'),
      excel('B1', 'Description'),
      excel('B2', 'Returns the character represented by the unicode number 66 (uppercase B).'),
      excel('B3', 'Returns the character represented by the unicode number 32 (space character).'),
      excel('B4', 'The unicode number 0 returns the error value #VALUE!'),
      excel('C1', 'Result'),

      shouldBe('A2', valueS('B')),
      shouldBe('A3', valueS('Space character')),
      shouldBe('A4', valueS('#VALUE!')),

      exec(done)
    ]);
  });
  it('UNICODE', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=UNICODE(" ")'),
      excel('A3', '=UNICODE("B")'),
      excel('B1', 'Description'),
      excel('B2', 'Returns the unicode number that the space character (a single space inside quotation marks) represents (32).'),
      excel('B3', 'Returns the unicode number that the uppercase "B" represents (66).'),
      excel('C1', 'Result'),

      shouldBe('A2', valueI(32)),
      shouldBe('A3', valueI(66)),

      exec(done)
    ]);
  });
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
  it('VALUE', (done) => {
    _do([
      excel('A1', 'Formula'),
      excel('A2', '=VALUE("$1,000")'),
      excel('A3', '=VALUE("16:48:00")-VALUE("12:00:00")'),
      excel('B1', 'Description'),
      excel('B2', 'Number equivalent of the text string "$1,000"'),
      excel('B3', 'The serial number equivalent to 4 hours and 48 minutes, which is "16:48:00" minus "12:00:00" (0.2 = 4:48).'),
      excel('C1', 'Result'),

      shouldBe('A2', valueI(1000)),
      shouldBe('A3', valueD(0.2)),

      exec(done)
    ]);
  });
});
