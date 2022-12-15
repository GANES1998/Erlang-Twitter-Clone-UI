import Request from "./Request";

export const signupService = async (username, password) => {
    const response = await Request.get('/signup', {params:{username, password}});
    return response.data;
}