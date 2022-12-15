import {Form, Formik} from "formik";
import * as yup from "yup"
import InputComponent from "../components/InputComponent";

const initialValue = {
    hashtag: ''
};

const SearchByHashtagForm = ({handleSubmit}) => {

    const validationSchema = yup.object({
        hashtag: yup.string().required()
    });

    return (
        <>
            <Formik initialValues={initialValue}
                    validationSchema={validationSchema}
                    onSubmit={handleSubmit}>
                {(formik) => (
                    <Form>
                        <InputComponent name={'hashtag'} placeholder={'Enter hashtag here'}/>
                        <button type={"submit"} disabled={!(formik.dirty && formik.isValid)}>Submit</button>
                    </Form>
                )}
            </Formik>
        </>
    )
}

export default SearchByHashtagForm;