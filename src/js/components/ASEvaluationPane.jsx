import React from 'react';
import ASCodeEditor from './ASCodeEditor.jsx';
import ASSpreadsheet from './ASSpreadsheet.jsx';
import ASEvaluationStore from '../stores/ASEvaluationStore';

function getEvaluationState () {
  return {
    currentCell: ASEvaluationStore.getCurrentCell() //TODO: rename it in the store file as well
  };
}

export default React.createClass({
  getInitialState() {
    return getEvaluationState();
  },

  componentDidMount() {
    ASEvaluationStore.addChangeListener(this._onChange);
  },

  componentWillUnmount() {
    ASEvaluationStore.removeChangeListener(this._onChange);
  },

  render() {
    let {language, expression} = this.state.currentCell;

    return (
      <div className="full">
        <ASCodeEditor mode={language} value={expression}
          width="100%" height="200px" />
        <ASSpreadsheet width="100%" height="100%" />
      </div>
    );
  },

  _onChange() {
    this.setState(getEvaluationState());
  }
});
