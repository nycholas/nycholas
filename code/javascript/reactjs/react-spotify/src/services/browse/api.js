import axios from 'axios';

export const newReleases = async () => {
  const uri = `/browse/new-releases`;
  const response = await axios.get(uri);
  const data = response.data;
  return data;
};
