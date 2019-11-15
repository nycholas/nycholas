import axios from 'axios';

export const recommendations = async () => {
  const uri = `/recommendations`;
  const response = await axios.get(uri);
  const data = response.data;
  return data;
};
