import axios from "axios";

const Request = axios.create();

Request.defaults.baseURL = "http://localhost:8080/"
Request.defaults.headers.common['Content-Type'] = 'application/json'
Request.defaults.headers.get['Content-Type'] = 'application/json'

export default Request