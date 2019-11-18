import axios from 'axios';

export const axiosConfig = () => {
  axios.defaults.baseURL = 'https://react-spotify.getsandbox.com';
  axios.defaults.headers.common['Content-Type'] = 'application/json';
};
