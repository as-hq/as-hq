// /* @flow */
//
// import type {
//   NakedRange,
//   ASIndexObject,
//   ColorCellTag,
//   ASCellObject,
//   ASCellProp
// } from '../../types/Eval';
//
// import React, {PropTypes} from 'react';
// import ASButton from './ASButton.jsx';
// import ASCheckedButton from './ASCheckedButton.jsx';
//
// import Util from '../../AS/Util';
// import TC from '../../AS/TypeConversions';
// import API from '../../actions/ASApiActionCreators';
// import CellStore from '../../stores/ASCellStore';
// import SheetStateStore from '../../stores/ASSheetStateStore';
// import SelectionStore from '../../stores/ASSelectionStore';
//
// import ASCellPropControl from './ASCellPropControl.jsx';
//
// type ToggleButtonProps = {
//   iconClassName: string;
//   propTag: ColorCellTag;
//   display: string;
// };
//
// type ToggleButtonState = {
//   color: ?string;
// };
//
// export default React.createClass({
//   propTypes: {
//     iconClassName: React.PropTypes.string.isRequired,
//     propTag: React.PropTypes.string.isRequired
//   },
//
//   getInitialState(): ToggleButtonState {
//     return ({
//       color: null
//     });
//   },
//
//   _setBackendCellProp(nextState: boolean, rng: NakedRange) {
//     let prop = (({ tag: this.props.propTag, contents: this.state.color}): any);
//     API.setProp(prop, rng);
//   },
//
//   _setControlStateFromCellProp(prop: ?ASCellProp) {
//     this.setState({color: prop.contents});
//   },
//
//   _onTouchTap() {
//     this.refs.controller.onControlStateChange(!this.state.active);
//   },
//
//   render(): React.Element {
//     let {iconClassName, propTag, ...etc} = this.props;
//     let {active} = this.state;
//
//     return (
//       <ASCellPropControl
//         ref="controller"
//         control={<ASCheckedButton
//           checked={active}
//           iconClassName={iconClassName}
//           onTouchTap={this._onTouchTap}
//           {...etc}
//         />}
//         setBackendCellProp={this._setBackendCellProp}
//         setControlStateFromCellProp={this._setControlStateFromCellProp}
//         propTag={this.props.propTag}/>
//     );
//   }
// });
