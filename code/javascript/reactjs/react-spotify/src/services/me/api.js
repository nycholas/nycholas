import axios from 'axios';

export const top5 = async () => {
  const uri = `/me/top`;
  const response = await axios.get(uri);
  const data = response.data;
  return data;
};
