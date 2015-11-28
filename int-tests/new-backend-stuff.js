          it('A:A should display ranges properly', (done) => {
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
          it('A2:A should display ranges properly', (done) => {
            _do([
              python('A1', '[range(10)]'),
              python('B1', 'A2:A'), 
              shouldBe('B1', valueI(1)), 
              shouldBe('B9', valueI(9)), 
              exec(done)
            ]);
          });
          it('A2:B should display ranges properly', (done) => {
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
          it('A:B should display ranges properly', (done) => {
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
          it('1:1 should display ranges properly', (done) => {
            _do([
              python('A1', '[range(10)]'),
              python('B1', '1:1'),
              shouldBe('B1', valueI(0)),
              shouldBe('B2', noValue()),
              exec(done)
            ]);
          });
          });
          it('Deleting ranges should work with A:A parsing', (done) => {
            _do([
              python('A1', '[range(10)]'),
              delete_('A1')
              delete_('A3')
              delete_('A4')
              delete_('A5')
              delete_('A8')
              delete_('A9')
              python('B1', 'A:A'), 
              shouldBe('B1', noValue()), 
              shouldBe('B2', valueI(1)), 
              shouldBe('B3', noValue()), 
              shouldBe('B9', noValue()), 
              shouldBe('B10', valueI(9)), 
              exec(done)
            ]);
          });

          it('should redo on Ctrl+Y after undo for A:A', (done) => {
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
