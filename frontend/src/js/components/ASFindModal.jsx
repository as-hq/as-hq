import React from 'react';
import {TextField, AppBar, FlatButton, FontIcon, Styles, Paper, Tab, Tabs} from 'material-ui';
import {findModal as findModalZIndex} from '../styles/zIndex';

/* NOTES:
using react-draggable around this is buggy; it jumps around while dragging and text selection drags as well
The point of this modal is to provide more options for find and replace (not yet implemented)
  case
  sheet
  full contents (the PayloadFind on backend covers these cases, and so does Excel)
replacing will be done on frontend and and eval request sent to backend after sending a find to backend
*/

function FindButtons({onNext, onPrev}) {
  return (
    <div style={styles.findButtons}>
      <FlatButton
        style={styles.findButton}
        onClick={onNext}
      >
        <FontIcon
          style={styles.icon}
          className="material-icons"
          color={Styles.Colors.blue50}
        >
          keyboard_arrow_down
        </FontIcon>
      </FlatButton>
      <FlatButton
        style={styles.findButton}
        onClick={onPrev}
      >
        <FontIcon
          style={styles.icon}
          className="material-icons"
          color={Styles.Colors.blue50}
        >
          keyboard_arrow_up
        </FontIcon>
      </FlatButton>
    </div>
  );
}


type FindModalProps = {
  initialSelection: number;
  onClose: () => void;
  onNext: () => void;
  onPrev: () => void;
};


export default function FindModal(props) {
  return (
    <div style={styles.findModal}>
      <Paper zDepth={5} style={styles.paper}>
        <AppBar
          style={styles.header}
          title="Find and Replace (TODO)"
          showMenuIconButton={false}
          iconElementRight={
            <FlatButton onClick={() => props.onClose()}>
              <FontIcon
                style={styles.icon}
                className="material-icons"
                color={Styles.Colors.grey900}
              >
                close
              </FontIcon>
            </FlatButton>
          }
        />
        <Tabs
          initialSelectedIndex={props.initialSelection}
          style={styles.tabs}
        >
          <Tab label="Find">
            <TextField
              style={styles.text}
              defaultValue=""
              floatingLabelText="Find Text"
              underlineStyle={{borderColor: Styles.Colors.amber900}}
            />
            <FindButtons
              onNext={props.onNext}
              onPrev={props.onPrev}
            />
          </Tab>
          <Tab label="Replace">
            <TextField
              style={styles.text}
              defaultValue=""
              floatingLabelText="Find Text"
              underlineStyle={{borderColor: Styles.Colors.amber900}}
            />
            <TextField
              style={styles.text}
              defaultValue=""
              floatingLabelText="Replace Text"
              underlineStyle={{borderColor: Styles.Colors.amber900}}
            />
          </Tab>
        </Tabs>
      </Paper>
    </div>
  );
}


const styles = {
  text: {
    width: '60%',
    paddingLeft: '20%',
    fontFamily: 'Roboto, sans-serif',
  },
  icon: {
    verticalAlign: 'middle',
  },
  findButtons: {
    textAlign: 'center',
    height: 40,
  },
  findButton: {
    width: '20%',
    height: '100%',
    display: 'inline-block',
  },
  findModal: {
    position: 'absolute',
    zIndex: findModalZIndex,
    top: 0,
    bottom: 0,
    left: 0,
    right: 0,
  },
  header: {
    height: 10,
    backgroundColor: Styles.Colors.amber900,
  },
  paper: {
    width: 500,
    height: 500,
    margin: '200px auto 0',
    top: 200,
    borderStyle: '2px solid',
  },
  tabs: {
    backgroundColor: Styles.Colors.grey900,
    height: '100%',
  },
};
