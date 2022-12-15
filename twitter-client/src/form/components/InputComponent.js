import classes from "./formcomp.module.scss";
import {useField} from "formik";

const InputComponent = ({name, placeholder, type="text"}) => {
    const [field, meta, helper] = useField(name);

    return (
        <div>
            <input type={type}
                   className={classes.formComponent}
                   placeholder={placeholder}
                   value={field.value}
                   onChange={(e) => helper.setValue(e.target.value)}/>
        </div>
    )
}

export default InputComponent