/* @flow */

import FFMap from '../form-generators/FormFMap.jsx';
import FSum from '../form-generators/FormSum.jsx';
import FTuple from '../form-generators/FormTuple.jsx';
import FObject from '../form-generators/FormObject.jsx';
import FConstant from '../form-generators/FormConstant.jsx';
import FAddendLift from '../form-generators/FormAddendLift.jsx';

import TabSelector from '../form-generators/TabSelector.jsx';
import HODropdownMenu from '../form-generators/HODropdownMenu.jsx';

import TextField from '../form-generators/FCTextField.jsx';
import ColorPicker from '../form-generators/FCColorPicker.jsx';

const LanguageForm =
  HODropdownMenu([
    'Python', 'Excel'
  ].map(
    (txt) => ({ payload: txt, text: txt })
  ));

const ExpressionForm = FObject({
  tag: 'Expression',
  format: [
    { key: 'expression', form: TextField },
    { key: 'language', form: LanguageForm }
  ]
});

const GreaterThanForm = FAddendLift({
  tag: 'GreaterThanCondition', text: 'Greater than', form: ExpressionForm
});

const LessThanForm = FAddendLift({
  tag: 'LessThanCondition', text: 'Less than', form: ExpressionForm
});

const IsBetweenForm = {
  tag: 'IsBetweenCondition',
  text: 'Is between',
  form: FTuple({
    tag: 'IsBetweenCondition',
    format: [
      { form: ExpressionForm },
      { form: ExpressionForm }
    ]
  })
};

const CustomConditionForm = FAddendLift({
  tag: 'CustomCondition', text: 'Custom condition', form: ExpressionForm
});

const ConditionForm = {
  key: 'boolFormatMapCondition',
  form: FSum({
    formats: [
      GreaterThanForm,
      LessThanForm,
      IsBetweenForm,
      CustomConditionForm
    ]
  })
};

const TextColorForm = FAddendLift({
  tag: 'TextColor', text: 'Text color', form: ColorPicker
});

const FillColorForm = FAddendLift({
  tag: 'FillColor', text: 'Fill color', form: ColorPicker
});

const FontSizeForm = FAddendLift({
  tag: 'FontSize', text: 'Font size', form: TextField
});

const CellPropForm = {
  key: 'boolFormatMapProps',
  form: FFMap(
    {
      f(lower) { return [lower]; },
      fInverse(upper) { return upper[0]; }
    },
    FSum({
      formats: [
        TextColorForm,
        FillColorForm,
        FontSizeForm,
        ...['Bold', 'Italic', 'Underline'].map((format) => {
          return {
            tag: format,
            text: format,
            form: FConstant({ contents: [] })
          }
        })
      ]
    })
  )
};

const BoolConditionForm = {
  tag: 'BoolFormatMapConstructor',
  text: 'Condition',
  form: FObject({
    tag: 'BoolFormatMapConstructor',
    format: [ConditionForm, CellPropForm]
  })
};

const LambdaForm = FAddendLift({
  tag: 'LambdaFormatMapConstructor',
  text: 'Lambda formatter',
  form: TextField // TODO: change to code editor compliant with valueLink
});

const Form = FSum({
  formats: [BoolConditionForm, LambdaForm],
  menuComponent: TabSelector
});

export default Form;
