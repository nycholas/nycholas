import React from 'react';
import { shallow } from 'enzyme';
import toJSON from 'enzyme-to-json';

import Footer from './index';

describe('<Footer/>', () => {
  it('Should match to snapshot', () => {
    const wrapper = shallow(<Footer />);
    const tree = toJSON(wrapper);
    expect(tree).toMatchSnapshot();
  });
});
