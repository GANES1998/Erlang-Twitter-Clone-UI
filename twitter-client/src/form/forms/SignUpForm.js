import {Form, Formik} from "formik";
import * as yup from "yup"
import InputComponent from "../components/InputComponent";
import {signupService} from "../../service/SignUpService";
import {useContext} from "react";
import {StaticDataContext} from "../../context/StaticDataContext";
import {useNavigate} from "react-router";

const SignUpForm = () => {
    const {loginUser} = useContext(StaticDataContext);
    const navigate = useNavigate();

    const initialValue = {
        username: '',
        password: '',
    };

    const validationSchema = yup.object({
            username: yup.string().required(),
            password: yup.string().required()
        });

    const handleSubmit = async (values) => {
        const loginId = await signupService(values.username, values.password);
        alert(loginId);
        loginUser(loginId);
        navigate('/');
    }

    return (
        <Formik initialValues={initialValue}
                validationSchema={validationSchema}
                onSubmit={handleSubmit}>
            {(formik) => (
                <Form>
                    <InputComponent name={'username'} placeholder={'User Name'} />
                    <InputComponent name={'password'} type={"password"} placeholder={'Password'} />
                    <button type={"submit"} disabled={!(formik.dirty && formik.isValid)}>Submit</button>
                </Form>
            )}
        </Formik>
    )
}

export default SignUpForm;